# GlobalMentor Java Model Library

Utilities for working with the Java language model API (`javax.lang.model`) and facilitating annotation processor development.

## Quick Reference

**Annotation processor base class:** `com.globalmentor.java.model.BaseAnnotationProcessor` — extends `AbstractProcessor` with convenient access to element/type utilities, automatic latest source version support, and flexible annotation type configuration.

**Element utilities:** `com.globalmentor.java.model.ModelElements` — find type elements by class or canonical name; retrieve present/declared annotation mirrors; create `Annotations` abstraction for model elements.

**Type utilities:** `com.globalmentor.java.model.ModelTypes` — type assignability testing via `isTypeAssignableTo()`; annotation value extraction; declared type lookup with wildcard support.

## Overview

### `com.globalmentor.java.model`

Utilities for the Java language model API (`javax.lang.model`), primarily for annotation processor development.

#### Base Annotation Processor

`BaseAnnotationProcessor` provides a foundation for annotation processors with:
- Automatic support for the latest source version
- Convenient access to `Elements` and `Types` utilities via instance methods
- Flexible annotation type specification (constructor or `@SupportedAnnotationTypes` annotation)
- Type assignability predicates for filtering elements

```java
public class MyProcessor extends BaseAnnotationProcessor {

    public MyProcessor() {
        super(Set.of(MyAnnotation.class));
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        for (Element element : roundEnv.getElementsAnnotatedWith(MyAnnotation.class)) {
            if (isTypeAssignableTo(element.asType(), Serializable.class)) {
                // process serializable annotated elements
            }
        }
        return true;
    }
}
```

#### Model Elements

`ModelElements` provides utilities for working with `Element` instances:

```java
// Find type element for a class
Optional<TypeElement> typeElement = ModelElements.findTypeElementForClass(elements, MyClass.class);

// Find annotation mirrors (present or declared)
Optional<? extends AnnotationMirror> mirror = 
    ModelElements.findElementAnnotationMirrorForClass(elements, element, MyAnnotation.class);

// Create Annotations abstraction for uniform annotation access
Annotations annotations = ModelElements.annotationsOf(elements, element);
if (annotations.isAnnotationPresent(Deprecated.class)) { /* ... */ }
```

#### Model Types

`ModelTypes` provides utilities for working with `TypeMirror` instances:

```java
// Test type assignability
boolean isSerializable = ModelTypes.isTypeAssignableTo(elements, types, typeMirror, Serializable.class);

// Create reusable assignability predicate
Predicate<TypeMirror> isCollection = ModelTypes.isTypeAssignableTo(elements, types, Collection.class);
elementTypes.stream().filter(isCollection).forEach(this::processCollection);

// Extract annotation values
Optional<? extends AnnotationValue> value = ModelTypes.findAnnotationValueElementValue(annotationMirror);

// Get declared types with wildcards
Optional<DeclaredType> listType = ModelTypes.findDeclaredTypeWithUnboundedWildcardForClass(elements, types, List.class);
```

## Issues

Issues tracked by [JIRA](https://globalmentor.atlassian.net/projects/JAVA).
