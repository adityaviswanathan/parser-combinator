package co.language.generator

import co.language.parser._
import co.language.runtime.{WorkflowAttributeTypeChecker}

object WorkflowGenerator {
	def apply(env: Map[String, Value]) = {
		entrypoint(env)
	}

	def entrypoint(env: Map[String, Value]) = {
		for(key <- env.keySet) {
			val envValue = env(key)
			envValue match {
				case ConstructorValue(construct) => {
					construct match {
						case App(attrs) => {
							for(attr <- attrs) {
								attr match {
									case AttributeToValue(key, value) => {
										if(key == "Entities") database(Seq(value), env)
										if(key == "Pages") ui(Seq(value), env)
									}
									case AttributeToList(key, values) => {
										if(key == "Entities") database(values, env)
										if(key == "Pages") ui(values, env)
									}
								}
							}
						} 
						case _ => {}
					}
				}
				case _ => {}
			}
		}
	}

	def database(entities: Seq[Value], env: Map[String, Value]) = {
		println("STARTING TO PARSE ENTITIES/PROPERTIES TO SETUP DATABASE")
		var metaDatabase: collection.mutable.Map[String, Any] = collection.mutable.Map[String, Any]()
		for(ent <- entities) {
			resolveAnd(ent, env, unwrapEntity) match {
				case Third(map) => { metaDatabase(getName(map)) = map	}
				case _ => {}
			}
			
		}

		// GET AWAY WITH HARD TYPECASTING FOR BREVITY SAKE BC DATA
		// HAS BEEN LINTED AND CHECKED ALREADY SO SCALA-STYLE 
		// TYPE CASCADE WOULD BE UNNECESSARILY VERBOSE HERE
		
		for((k,v) <- metaDatabase) {
			println("CREATE TABLE " + k)
			val m = v.asInstanceOf[collection.mutable.Map[String, Seq[Any]]] 
			for(prop <- m("Properties")) {
				val m2 = prop.asInstanceOf[collection.mutable.Map[String, Seq[Any]]]
				println("\tCREATE PROPERTY " + getName(m2))
			}
		}
		
	}

	def ui(pages: Seq[Value], env: Map[String, Value]) = {
		println("STARTING TO PARSE PAGES TO SETUP UI")
	}

	sealed trait OneOf[A,B,C]
	case class First[A,B,C](a: A) extends OneOf[A,B,C]
	case class Second[A,B,C](b: B) extends OneOf[A,B,C]
	case class Third[A,B,C](c: C) extends OneOf[A,B,C]

	def getName(attrs: collection.mutable.Map[String, Seq[Any]]): String = {
		attrs("Name")(0) match { 
			case s: String => { return s } 
			case _ => { return "" }
		}
	}

	def resolveVariable(resolvee: Value, env: Map[String, Value]): OneOf[String, Enum, Constructor] = {
		var resolved = resolvee
		while(true) {
			resolved match {
				case VariableValue(value) => { resolved = env(value) }
				case StringValue(value) => { return First(value) }
				case EnumValue(value) => { return Second(value) }
				case ConstructorValue(value) => { return Third(value) }
			}
		}
		return First("")
	}

	def resolveAnd(resolvee: Value, env: Map[String, Value], and: (Any, Map[String, Value]) => OneOf[String, Enum, collection.mutable.Map[String, Seq[Any]]]): OneOf[String, Enum, collection.mutable.Map[String, Seq[Any]]] = {
		resolveVariable(resolvee, env) match {
			case First(str) => { return and(str, env) }
			case Second(enum) => { return and(enum, env) }
			case Third(constructor) => { return and(constructor, env) }
		}
		return First("")
	}

	def unwrapEntity(ent: Any, env: Map[String, Value]): OneOf[String, Enum, collection.mutable.Map[String, Seq[Any]]] = {
		var entityMeta: collection.mutable.Map[String, Seq[Any]] = collection.mutable.Map[String, Seq[Any]]()
		ent match {
			case Entity(attrs) => {
				for(attr <- attrs) {
					attr match {
						case AttributeToValue(name, attr) => { 
							resolveAnd(attr, env, unwrapProperty) match {
								case First(str) => { entityMeta(name) = Seq(str) }
								case Second(enum) => { entityMeta(name) = Seq(enum) }
								case Third(map) => { entityMeta(name) = Seq(map) }
							}
						}
						case AttributeToList(name, attrs2) => {
							for(attr2 <- attrs2) {
								resolveAnd(attr2, env, unwrapProperty) match {
									case First(str) => { if(entityMeta.contains(name)) entityMeta(name) = entityMeta(name) ++ Seq(str) else entityMeta(name) = Seq(str) }
									case Second(enum) => { if(entityMeta.contains(name)) entityMeta(name) = entityMeta(name) ++ Seq(enum) else entityMeta(name) = Seq(enum) }
									case Third(map) => { if(entityMeta.contains(name)) entityMeta(name) = entityMeta(name) ++ Seq(map) else entityMeta(name) = Seq(map) }
								}
							}
						}
					}
				}
			}	
			case _ => {}
		}
		return Third(entityMeta)
	}

	def unwrapAttribute(attr: Any, env: Map[String, Value]): OneOf[String, Enum, collection.mutable.Map[String, Seq[Any]]] = {
		var attrMeta: collection.mutable.Map[String, Seq[Any]] = collection.mutable.Map[String, Seq[Any]]()
		attr match {
			case s: String => { return First(s) }
			case Enum => { return Second(attr.asInstanceOf[Enum]) }
			case _ => { return Third(attrMeta) }
		}
		return Third(attrMeta)
	}

	def unwrapProperty(prop: Any, env: Map[String, Value]): OneOf[String, Enum, collection.mutable.Map[String, Seq[Any]]] = {
		var propertyMeta: collection.mutable.Map[String, Seq[Any]] = collection.mutable.Map[String, Seq[Any]]()
		prop match {
			case Property(attrs) => {
				for(attr <- attrs) {
					attr match {
						case AttributeToValue(name, attr) => {
							resolveAnd(attr, env, unwrapAttribute) match {
								case First(str) => { propertyMeta(name) = Seq(str) }
								case Second(enum) => { propertyMeta(name) = Seq(enum) }
								case Third(map) => { propertyMeta(name) = Seq(map) }
							}
						}
						case AttributeToList(name, attrs2) => {
							for(attr2 <- attrs2) {
								resolveAnd(attr2, env, unwrapAttribute) match {
									case First(str) => { if(propertyMeta.contains(name)) propertyMeta(name) = propertyMeta(name) ++ Seq(str) else propertyMeta(name) = Seq(str) }
									case Second(enum) => { if(propertyMeta.contains(name)) propertyMeta(name) = propertyMeta(name) ++ Seq(enum) else propertyMeta(name) = Seq(enum) }
									case Third(map) => { if(propertyMeta.contains(name)) propertyMeta(name) = propertyMeta(name) ++ Seq(map) else propertyMeta(name) = Seq(map) }
								}
							}
						}
					}
				}
			}
			case s: String => { return First(s) }
			case Enum => { return Second(prop.asInstanceOf[Enum]) }
		}
		return Third(propertyMeta)
	}

}