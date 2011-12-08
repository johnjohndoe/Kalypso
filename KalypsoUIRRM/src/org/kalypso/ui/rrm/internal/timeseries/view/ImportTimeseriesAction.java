/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.ui.rrm.internal.timeseries.view;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.timeseries.binding.Station;
import org.kalypso.ui.rrm.internal.timeseries.binding.Timeseries;

/**
 * @author Gernot Belger
 */
public class ImportTimeseriesAction extends Action
{
  private final Station m_station;

  private final String m_parameterType;

  public ImportTimeseriesAction( final Station station, final String parameterType )
  {
    m_station = station;
    m_parameterType = parameterType;

    setText( "Import Timeseries" );
    setToolTipText( "Imports a timeseries from an external data source and adds it to the selected station." );

    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.IMPORT_TIMESERIES ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    try
    {
      final TimeseriesBean bean = new TimeseriesBean( null );

      showWizard( shell );
      // show wizard

      final IObservation observation;
      // import file
      importDataFile( bean, null );

      // create timeseries feature and set properties

      // select tree with pseudo node
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      StatusDialog.open( shell, e.getStatus(), getText() );
    }
    catch( final SensorException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  private void showWizard( final Shell shell )
  {
    final Wizard wizard = new TimeseriesImportWizard();
    final WizardDialog dialog = new WizardDialog( shell, wizard );
    dialog.open();
  }

  private void importDataFile( final TimeseriesBean bean, final IObservation observation ) throws CoreException, SensorException
  {
    final IFile dataFile = createDataFile( bean );

    ZmlFactory.writeToFile( observation, dataFile );
  }

  private IFile createDataFile( final TimeseriesBean bean ) throws CoreException
  {
    final Object parameterType = bean.getProperty( Timeseries.PROPERTY_PARAMETER_TYPE );
    final Object quality = bean.getProperty( Timeseries.PROPERTY_QUALITY );
    final String periodText = bean.getPeriodText();

    final String stationFoldername = m_station.getTimeseriesFoldername();
    final String timeseriesFilename = String.format( "%s_%s_%s.zml", parameterType, periodText, quality );

    final SzenarioDataProvider scenarioDataProvider = ScenarioHelper.getScenarioDataProvider();
    final IProject project = scenarioDataProvider.getScenarioFolder().getProject();
    final IFolder timeseriesFolder = project.getFolder( INaProjectConstants.PATH_TIMESERIES );
    final IFolder stationFolder = timeseriesFolder.getFolder( stationFoldername );

    return stationFolder.getFile( timeseriesFilename );
  }
}