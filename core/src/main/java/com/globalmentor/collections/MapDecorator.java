
package com.globalmentor.collections;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;

import static java.util.Objects.*;

/**
 * A map that wraps an existing map, providing access through the {@link Map} interface.
 * @param <K> The type of map key.
 * @param <V> The type of map value.
 * @author Garret Wilson
 */
public class MapDecorator<K, V> implements Map<K, V> {

	/** The map this class decorates. */
	protected final Map<K, V> map;

	/**
	 * Map constructor.
	 * @param map The map this map should decorate.
	 * @throws NullPointerException if the provided map is <code>null</code>.
	 */
	public MapDecorator(final Map<K, V> map) {
		this.map = requireNonNull(map, "Map cannot be null"); //save the map
	}

	@Override
	public int size() {
		return map.size();
	}

	@Override
	public boolean isEmpty() {
		return map.isEmpty();
	}

	@Override
	public boolean containsKey(Object key) {
		return map.containsKey(key);
	}

	@Override
	public boolean containsValue(Object value) {
		return map.containsValue(value);
	}

	@Override
	public V get(Object key) {
		return map.get(key);
	}

	// Modification Operations

	@Override
	public V put(K key, V value) {
		return map.put(key, value);
	}

	@Override
	public V remove(Object key) {
		return map.remove(key);
	}

	// Bulk Operations

	@Override
	public void putAll(Map<? extends K, ? extends V> t) {
		map.putAll(t);
	}

	@Override
	public void clear() {
		map.clear();
	}

	// Views

	@Override
	public Set<K> keySet() {
		return map.keySet();
	}

	@Override
	public Collection<V> values() {
		return map.values();
	}

	@Override
	public Set<Map.Entry<K, V>> entrySet() {
		return map.entrySet();
	}

	// Comparison and hashing

	@Override
	public boolean equals(Object o) {
		return map.equals(o);
	}

	@Override
	public int hashCode() {
		return map.hashCode();
	}

	// Default methods

	@Override
	public V getOrDefault(Object key, V defaultValue) {
		return map.getOrDefault(key, defaultValue);
	}

	@Override
	public void forEach(BiConsumer<? super K, ? super V> action) {
		map.forEach(action);
	}

	@Override
	public void replaceAll(BiFunction<? super K, ? super V, ? extends V> function) {
		map.replaceAll(function);
	}

	@Override
	public V putIfAbsent(K key, V value) {
		return map.putIfAbsent(key, value);
	}

	@Override
	public boolean remove(Object key, Object value) {
		return map.remove(key, value);
	}

	@Override
	public boolean replace(K key, V oldValue, V newValue) {
		return map.replace(key, oldValue, newValue);
	}

	@Override
	public V replace(K key, V value) {
		return map.replace(key, value);
	}

	@Override
	public V computeIfAbsent(K key, Function<? super K, ? extends V> mappingFunction) {
		return map.computeIfAbsent(key, mappingFunction);
	}

	@Override
	public V computeIfPresent(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
		return map.computeIfPresent(key, remappingFunction);
	}

	@Override
	public V compute(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
		return map.compute(key, remappingFunction);
	}

	@Override
	public V merge(K key, V value, BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
		return map.merge(key, value, remappingFunction);
	}

}
