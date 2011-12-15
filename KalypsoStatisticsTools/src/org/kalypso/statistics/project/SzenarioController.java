/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.statistics.project;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.kalypso.afgui.model.IModel;
import org.kalypso.afgui.scenarios.IScenario;
import org.kalypso.afgui.scenarios.IScenarioDataListener;
import org.kalypso.statistics.db.datasource.DataSourceManager;
import org.kalypso.statistics.db.datasource.DataSourceManager.DataSourceType;
import org.kalypso.statistics.gui.PartManager;
import org.kalypso.statistics.logger.ISysLogger;
import org.kalypso.statistics.utils.AppUtils;

/**
 * A central place for controlling scenario specific stuff for
 * KalypsoStatistics.
 * <p>
 * Get informed when models are loaded and/or scenario is changed.
 * 
 * @author Dejan Antanaskovic
 * @author Gernot Belger
 */
public class SzenarioController implements IScenarioDataListener {
	private IScenario m_scenario = null;

	public SzenarioController() {
	}

	@Override
	public synchronized void modelLoaded(final IModel model, final IStatus status) {
		if (!isScenarioValid()) {
			return;
		}

		// maybe get status info from status-model
	}

	public String getScenarioID() {
		if (isScenarioValid()) {
			return String.format("%s_%s", m_scenario.getProject().getName(), m_scenario.getScenario().getName());
		} else {
			return null;
		}
	}

	public boolean isScenarioValid() {
		if (m_scenario == null) {
			return false;
		}
		try {
			final IProject project = m_scenario.getProject();
			if (project.getNature(KalypsoStatisticsProjectNature.ID) == null) {
				return false;
			}
		} catch (CoreException e) {
			return false;
		}
		return true;
	}

	@Override
	public synchronized void scenarioChanged(final IScenario scenario) {
		// unregister any listeners
		m_scenario = scenario;
		if (!isScenarioValid()) {
			return;
		}
		final ISysLogger logger = AppUtils.getLogger();
		logger.logInfo(String.format("Scenario %s is loading...", scenario.getName()));
		// check if database is created
		if(DataSourceManager.getDataSource().isDataSourceRunning()){
			DataSourceManager.getDataSource().stopDataSource();
		}
		DataSourceManager.setDataSourceType(DataSourceType.DERBY_EMBEDDED);
		DataSourceManager.getDataSource().startDataSource(scenario);
		if (DataSourceManager.getDataSource().isDataSourceErrorOccured()) {
			logger.logError("Error while starting database.");
			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					MessageDialog.openError(Display.getDefault().getActiveShell(), AppUtils.APPLICATION_TITLE, "Error while starting database.");
				}
			});
			return;
		}
		checkDataSourceInitialized();
		if (DataSourceManager.getDataSource().isDataSourceErrorOccured()) {
			logger.logError("Error while initializing database.");
			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					MessageDialog.openError(Display.getDefault().getActiveShell(), AppUtils.APPLICATION_TITLE, "Error while initializing database.");
				}
			});
			return;
		}
		Job job = new Job("") {
			@Override
			protected IStatus run(IProgressMonitor arg0) {
				SessionDataProvider.getInstance().loadScenario(getScenarioID());
				PartManager.getInstance().initialize();
				return Status.OK_STATUS;
			}
		};
		job.setSystem(true);
		job.setUser(false);
		job.setPriority(Job.SHORT);
		job.schedule();
	}

	private void checkDataSourceInitialized() {
		boolean isDataSourceInitialized = DataSourceManager.getDataSource().isDataSourceInitialized();
		if (!isDataSourceInitialized) {
			DataSourceManager.getDataSource().initDataSource();
		}
		if (isDataSourceInitialized && DataSourceManager.getDataSource().isDataSourceUpdateAvailable()) {
			DataSourceManager.getDataSource().updateDataSource();
		}
	}
}
