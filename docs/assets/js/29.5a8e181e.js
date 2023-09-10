(window.webpackJsonp=window.webpackJsonp||[]).push([[29],{311:function(t,a,e){"use strict";e.r(a);var s=e(14),n=Object(s.a)({},(function(){var t=this,a=t._self._c;return a("ContentSlotsDistributor",{attrs:{"slot-key":t.$parent.slotKey}},[a("h1",{attrs:{id:"federation"}},[a("a",{staticClass:"header-anchor",attrs:{href:"#federation"}},[t._v("#")]),t._v(" Federation")]),t._v(" "),a("p",[a("strong",[t._v("Federation")]),t._v(" is an optional module which can be included in your configuration to enroll with a federated schema.")]),t._v(" "),a("h2",{attrs:{id:"dependencies"}},[a("a",{staticClass:"header-anchor",attrs:{href:"#dependencies"}},[t._v("#")]),t._v(" Dependencies")]),t._v(" "),a("p",[a("code",[t._v("caliban-federation")]),t._v(" only depends on "),a("code",[t._v("caliban-core")]),t._v(" and is very unobtrusive.")]),t._v(" "),a("p",[t._v("To use, add the following dependency to your "),a("code",[t._v("build.sbt")]),t._v(" file:")]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"com.github.ghostdogpr"')]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("%")]),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("%")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"caliban-federation"')]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("%")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"2.3.0"')]),t._v("\n")])])]),a("h2",{attrs:{id:"federating"}},[a("a",{staticClass:"header-anchor",attrs:{href:"#federating"}},[t._v("#")]),t._v(" Federating")]),t._v(" "),a("p",[t._v("Federation allows graphs to become part of a larger graph without having to share models or create brittle\nschema stitching code at the gateway level.")]),t._v(" "),a("p",[t._v("You can read more about federation and why it may be useful "),a("a",{attrs:{href:"https://www.apollographql.com/docs/apollo-server/federation/introduction/",target:"_blank",rel:"noopener noreferrer"}},[t._v("here"),a("OutboundLink")],1),t._v(".")]),t._v(" "),a("p",[t._v("Federation creates a wrapper over your existing schema so that it can add the necessary hooks to support\ninteraction with the gateway.")]),t._v(" "),a("p",[t._v("If you already have a graph you can add federation simply by adding the "),a("code",[t._v("federated")]),t._v(" annotation:")]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("import")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token namespace"}},[t._v("caliban"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("federation"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("v1"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")])]),t._v("_\n\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" schema"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" GraphQL"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("R"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" graphQL"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("RootResolver"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("Queries"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("\n  characters "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" List"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("Character"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"Amos"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" federatedSchema"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" GraphQL"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("R"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" schema @@ federated\n")])])]),a("p",[t._v("This will wrap the bare minimum schema additions around your API so that the gateway will recognize your schema.\nTo actually enable entity resolution you will need to do a bit of leg work.")]),t._v(" "),a("p",[t._v('First, any types that will be "resolvable" need to be annotated with a '),a("code",[t._v("@key")]),t._v(" directive. You can use a helper function found\nin the "),a("code",[t._v("federation")]),t._v(" package to help with that.")]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[a("span",{pre:!0,attrs:{class:"token annotation punctuation"}},[t._v("@GQLKey")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"name"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" Character"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("name"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("String")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n")])])]),a("p",[t._v("The "),a("code",[t._v('"name"')]),t._v(" field is a field selector minus the outer braces.")]),t._v(" "),a("p",[t._v("If you need to extend a type from another service, you will need to define a stub version of it in the current service\nand annotate it with the "),a("code",[t._v("@extends")]),t._v(" annotation")]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[a("span",{pre:!0,attrs:{class:"token annotation punctuation"}},[t._v("@GQLKey")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"season episode"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" \n"),a("span",{pre:!0,attrs:{class:"token annotation punctuation"}},[t._v("@GQLExtend")]),t._v("\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" Episode"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token annotation punctuation"}},[t._v("@GQLExternal")]),t._v(" season"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Int")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token annotation punctuation"}},[t._v("@GQLExternal")]),t._v(" episode"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Int")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" cast"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" List"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("Character"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n")])])]),a("p",[t._v("Note the additional annotations we needed in this case. "),a("code",[t._v("Extend")]),t._v(" is needed to tell the gateway that this type is defined within\nanother service, while the "),a("code",[t._v("External")]),t._v(" flags these fields as being owned by the other service (there are several other annotations\navailable that you are encouraged to read about).")]),t._v(" "),a("p",[t._v("Once you have annotated your types, you need to tell "),a("code",[t._v("Federation")]),t._v(" how to resolve those types. Federation uses a slightly\ndifferent mechanism in resolving types from a standard GraphQL query, so for each type that you wish to support, you will\nneed to add an "),a("code",[t._v("EntityResolver")]),t._v(":")]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[t._v("EntityResolver"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("CharacterService"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" CharacterArgs"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" Character"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("args "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("=>")]),t._v(" \n  ZQuery"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("fromEffect"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("characters"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("getCharacter"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("args"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("name"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("  \n")])])]),a("p",[a("code",[t._v("EntityResolvers")]),t._v(' like normal field resolvers also supports a "metadata" variant which can be used to inspect the requested\nfields and potentially optimize the resulting query. You can use the provided helper method if you need to access the metadata field:')]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[t._v("EntityResolver"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("fromMetadata"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("CharacterArgs"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("field "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("=>")]),t._v(" args "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("=>")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("if")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("field"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("fields"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("forall"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("_"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("name "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("==")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"name"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" ZQuery"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("succeed"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("Character"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("args"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("name"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" Nil"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" None"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("else")]),t._v(" ZQuery"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("fromEffect"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("characters"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("getCharacter"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("args"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("name"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n")])])]),a("p",[t._v("In the above we need to define an resolver which takes an "),a("code",[t._v("R")]),t._v(" environment type,\nan "),a("code",[t._v("A")]),t._v(" which has an implicit "),a("code",[t._v("ArgBuilder")]),t._v(" and an "),a("code",[t._v("Option[Out]")]),t._v(" where "),a("code",[t._v("Out")]),t._v(" has an implicit\n"),a("code",[t._v("Schema[R, Out]")]),t._v(" available. Creating the above we can now add these resolvers to our federated schema like so:")]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[t._v("schema @@ federated"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("aResolver"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" additionalResolvers"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v("_"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("*")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n")])])]),a("p",[t._v("You can now use the resulting "),a("code",[t._v("GraphQL[R]")]),t._v(" to start querying. You can also see the full code example "),a("a",{attrs:{href:"https://github.com/ghostdogpr/caliban/tree/series/2.x/examples/src/main/scala/example/federation",target:"_blank",rel:"noopener noreferrer"}},[t._v("here"),a("OutboundLink")],1)]),t._v(" "),a("h2",{attrs:{id:"tracing"}},[a("a",{staticClass:"header-anchor",attrs:{href:"#tracing"}},[t._v("#")]),t._v(" Tracing")]),t._v(" "),a("p",[t._v("Federated tracing is slightly different from standard apollo-tracing thus it comes with its own wrapper defined in the "),a("code",[t._v("caliban-federation")]),t._v(" module.")]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("import")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token namespace"}},[t._v("caliban"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("federation"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("tracing"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")])]),t._v("ApolloFederatedTracing\n\n\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" api "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" schema @@ federated"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("resolver"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" additionalResolvers"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" _"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("*")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" @@ ApolloFederatedTracing"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("wrapper\n")])])]),a("p",[t._v("In federated tracing the gateway communicates with the implementing service via a header "),a("code",[t._v("apollo-federation-include-trace")]),t._v(",\nfor now the only value it can send is "),a("code",[t._v("ftv1")]),t._v(". Thus if you detect this header then you should enable tracing otherwise you can disable it.")]),t._v(" "),a("p",[t._v("If you are using one of the wrappers you are done, they will automatically detect when the gateway\nenables tracing on a request. However, if you are calling the "),a("code",[t._v("interpreter.execute")]),t._v(" independently or you have some other custom\nset up you will need to add one more step to enable tracing.")]),t._v(" "),a("p",[t._v("If you wish to enable it manually (after detecting the header with your preferred framework) you can call: "),a("code",[t._v("request.withFederatedTracing")]),t._v(" which will return a new "),a("code",[t._v("GraphQLRequest")]),t._v(" that informs the wrapper\nthat it should include tracing data as part of the response extensions.")]),t._v(" "),a("h2",{attrs:{id:"federation-v2"}},[a("a",{staticClass:"header-anchor",attrs:{href:"#federation-v2"}},[t._v("#")]),t._v(" Federation V2")]),t._v(" "),a("p",[t._v("Caliban can support the v2 federation specification as well. If your gateway supports the "),a("a",{attrs:{href:"https://www.apollographql.com/docs/federation/federation-spec",target:"_blank",rel:"noopener noreferrer"}},[t._v("Federation V2 specification"),a("OutboundLink")],1),t._v(", you can specify the supported feature set\nby using "),a("code",[t._v("caliban.federation.v2_x")]),t._v(" where "),a("code",[t._v("x")]),t._v(" is the minor version of the specification you wish to use.")]),t._v(" "),a("table",[a("thead",[a("tr",[a("th",[t._v("Directive")]),t._v(" "),a("th",[t._v("Caliban Type")]),t._v(" "),a("th",[t._v("Version")]),t._v(" "),a("th",[t._v("Caliban package")])])]),t._v(" "),a("tbody",[a("tr",[a("td",[a("code",[t._v("@shareable")])]),t._v(" "),a("td",[a("code",[t._v("@GQLShareable")])]),t._v(" "),a("td",[t._v("v2.0")]),t._v(" "),a("td",[a("code",[t._v("caliban.federation.v2_0")])])]),t._v(" "),a("tr",[a("td",[a("code",[t._v("@inaccessable")])]),t._v(" "),a("td",[a("code",[t._v("@GQLInaccessible")])]),t._v(" "),a("td",[t._v("v2.0")]),t._v(" "),a("td",[a("code",[t._v("caliban.federation.v2_0")])])]),t._v(" "),a("tr",[a("td",[a("code",[t._v("@override")])]),t._v(" "),a("td",[a("code",[t._v("@GQLOverride")])]),t._v(" "),a("td",[t._v("v2.0")]),t._v(" "),a("td",[a("code",[t._v("caliban.federation.v2_0")])])]),t._v(" "),a("tr",[a("td",[a("code",[t._v("@tag")])]),t._v(" "),a("td",[a("code",[t._v("@GQLTag")])]),t._v(" "),a("td",[t._v("v2.0")]),t._v(" "),a("td",[a("code",[t._v("caliban.federation.v2_0")])])]),t._v(" "),a("tr",[a("td",[a("code",[t._v("@composeDirective")])]),t._v(" "),a("td",[a("code",[t._v("ComposeDirective")])]),t._v(" "),a("td",[t._v("v2.1")]),t._v(" "),a("td",[a("code",[t._v("caliban.federation.v2_1")])])]),t._v(" "),a("tr",[a("td",[a("code",[t._v("@interfaceObject")])]),t._v(" "),a("td",[a("code",[t._v("@GQLInterfaceObject")])]),t._v(" "),a("td",[t._v("v2.3")]),t._v(" "),a("td",[a("code",[t._v("caliban.federation.v2_3")])])])])]),t._v(" "),a("p",[t._v("The "),a("code",[t._v("GQLKey")]),t._v(" field now also supports the "),a("code",[t._v("resolvable")]),t._v(" argument.")]),t._v(" "),a("p",[t._v("Using the new "),a("code",[t._v("federated")]),t._v(" aspect from any v2_x package will automatically make your graph available as a v2 schema,\neven if you aren't using the new directives.")]),t._v(" "),a("p",[t._v("For more information see the "),a("a",{attrs:{href:"https://www.apollographql.com/docs/federation/federation-2/new-in-federation-2/",target:"_blank",rel:"noopener noreferrer"}},[t._v("Federation V2 specification"),a("OutboundLink")],1),t._v(".")]),t._v(" "),a("h3",{attrs:{id:"customizing-federation"}},[a("a",{staticClass:"header-anchor",attrs:{href:"#customizing-federation"}},[t._v("#")]),t._v(" Customizing Federation")]),t._v(" "),a("p",[t._v("Federation 2.1 introduced a new schema level directive called "),a("code",[t._v("@composeDirective")]),t._v(" which allows you to specify custom directives that should\nbe visible to clients of the gateway (by default all directives are hidden to clients of the gateway)")]),t._v(" "),a("p",[t._v("GraphQL federation is an evolving specification and not all routers support all features.\nCaliban provides support for "),a("code",[t._v("v2.0")]),t._v(", "),a("code",[t._v("v2.1")]),t._v(" and "),a("code",[t._v("v2.3")]),t._v(" of the specification. If you need to use\nan earlier version or you need to customize some aspect of the federation directives (for instance by providing your own "),a("code",[t._v("@composeDirective")]),t._v("s) you can do so by simply extending the "),a("code",[t._v("FederationV2")]),t._v(" class.")]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[a("span",{pre:!0,attrs:{class:"token comment"}},[t._v("// With a package object but you can also create a normal object")]),t._v("\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("package")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token namespace"}},[t._v("object")]),t._v(" myFederation "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("extends")]),t._v(" FederationV2"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("\n  Versions"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("v2_3 "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("::")]),t._v(" \n    Link"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"https://myspecs.dev/myDirective/v1.0"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" List"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("\n      Import"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"@myDirective"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v("\n      Import"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"@anotherDirective"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" as "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" Some"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"@hello"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n    "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("::")]),t._v(" \n      ComposeDirective"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"@myDirective"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("::")]),t._v(" \n      ComposeDirective"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"@hello"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("::")]),t._v(" Nil\n    "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("with")]),t._v(" FederationDirectivesV2_3\n\n"),a("span",{pre:!0,attrs:{class:"token comment"}},[t._v("// Then import your new federation object instead of `caliban.federation.v2_3`")]),t._v("\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("import")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token namespace"}},[t._v("myFederation"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")])]),t._v("_\n")])])])])}),[],!1,null,null,null);a.default=n.exports}}]);