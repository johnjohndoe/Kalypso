package org.kalypso.statistics.project;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.ui.PlatformUI;
import org.kalypso.statistics.db.DataProvider;
import org.kalypso.statistics.db.handler.DBHandlerSysVars;
import org.kalypso.statistics.gui.GlobalListenersManager;
import org.kalypso.statistics.gui.PartManager;

public class SessionDataProvider {

	private static SessionDataProvider INSTANCE = null;

	private final Map<String, String> m_sysVars = new HashMap<String, String>();

	private final ISchedulingRule m_schedulingRule = new ISchedulingRule() {
		@Override
		public boolean isConflicting(final ISchedulingRule rule) {
			return rule == this;
		}

		@Override
		public boolean contains(final ISchedulingRule rule) {
			return rule == this;
		}
	};

	private SessionDataProvider() {
		// singleton
		DBHandlerSysVars.loadVars(m_sysVars);
		// unloadSession();
	}

	public static SessionDataProvider getInstance() {
		if (INSTANCE == null) {
			synchronized (SessionDataProvider.class) {
				INSTANCE = new SessionDataProvider();
			}
		}
		return INSTANCE;
	}

	public DataProvider getDataProvider() {
		return DataProvider.getInstance();
	}

	public void loadScenario(final String scenarioID) {
		if (scenarioID == null) {
			return;
		}
		loadSession();
	}

	private void loadSession() {
		getDataProvider().loadHandlers();
		GlobalListenersManager.getInstance().informAllListeners();
	}

	public ISchedulingRule getSchedulingRule() {
		return m_schedulingRule;
	}

	public static boolean isShutdownInProgress() {
		return PlatformUI.isWorkbenchRunning();
	}

}
