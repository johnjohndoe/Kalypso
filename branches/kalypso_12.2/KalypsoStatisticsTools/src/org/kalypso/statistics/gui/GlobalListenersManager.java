package org.kalypso.statistics.gui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.statistics.types.data.AbstractStatisticsRecordType;
import org.kalypso.statistics.types.data.NodeProfile;
import org.kalypso.statistics.types.data.TimeserieProfile;

public class GlobalListenersManager {

	private static GlobalListenersManager INSTANCE = null;

	public static enum RecordChangeListenerType {
		NODES, TIMESERIES
	}

	private final Map<RecordChangeListenerType, List<IStatisticsRecordChangeListener>> m_recordChangeListeners = new HashMap<GlobalListenersManager.RecordChangeListenerType, List<IStatisticsRecordChangeListener>>();

	private GlobalListenersManager() {
	}

	public static GlobalListenersManager getInstance() {
		if (INSTANCE == null) {
			INSTANCE = new GlobalListenersManager();
		}
		return INSTANCE;
	}

	public void registerRecordChangeListener(final RecordChangeListenerType listenerType, final IStatisticsRecordChangeListener listener) {
		if (!m_recordChangeListeners.containsKey(listenerType)) {
			final List<IStatisticsRecordChangeListener> list = new ArrayList<IStatisticsRecordChangeListener>();
			m_recordChangeListeners.put(listenerType, list);
			list.add(listener);
		} else {
			final List<IStatisticsRecordChangeListener> list = m_recordChangeListeners.get(listenerType);
			if (!list.contains(listener)) {
				list.add(listener);
			}
		}
	}

	public void unregisterRecordChangeListener(final RecordChangeListenerType listenerType, final IStatisticsRecordChangeListener listener) {
		if (m_recordChangeListeners.containsKey(listenerType)) {
			m_recordChangeListeners.get(listenerType).remove(listener);
		}
	}

	private void onRecordChange(final RecordChangeListenerType recordChangeType) {
		if (m_recordChangeListeners.containsKey(recordChangeType)) {
			for (final IStatisticsRecordChangeListener listener : m_recordChangeListeners.get(recordChangeType)) {
				listener.onRecordChanged();
			}
		}
	}

	public void informAllListeners() {
		for (final RecordChangeListenerType value : RecordChangeListenerType.values()) {
			GlobalListenersManager.getInstance().onRecordChange(value);
		}
	}

	public void informListeners(final Class<? extends AbstractStatisticsRecordType> recordClass) {
		final List<Class<?>> interfaces = new ArrayList<Class<?>>();
		if (recordClass.isInterface()) {
			interfaces.add(recordClass);
		} else {
			final Class<?>[] myInterfaces = recordClass.getInterfaces();
			for (final Class<?> iface : myInterfaces) {
				interfaces.add(iface);
			}
		}
		for (final Class<?> i : interfaces) {
			if (i.equals(NodeProfile.class)) {
				GlobalListenersManager.getInstance().onRecordChange(GlobalListenersManager.RecordChangeListenerType.NODES);
			}
			if (i.equals(TimeserieProfile.class)) {
				GlobalListenersManager.getInstance().onRecordChange(GlobalListenersManager.RecordChangeListenerType.TIMESERIES);
			}
		}
	}
}