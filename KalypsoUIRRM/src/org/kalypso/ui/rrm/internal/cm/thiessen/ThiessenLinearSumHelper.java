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
package org.kalypso.ui.rrm.internal.cm.thiessen;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IThiessenStationCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.cm.view.InitThiessenTimeseriesOperation;
import org.kalypso.ui.rrm.internal.cm.view.LinearSumBean;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * @author Gernot Belger
 */
public final class ThiessenLinearSumHelper
{
  private ThiessenLinearSumHelper( )
  {
    throw new UnsupportedOperationException();
  }

  public static LinearSumBean createFromCurrentScenario( final String parameterType )
  {
    try
    {
      final SzenarioDataProvider scenarioDataProvider = ScenarioHelper.getScenarioDataProvider();
      final NaModell model = scenarioDataProvider.getModel( IUiRrmWorkflowConstants.SCENARIO_DATA_MODEL, NaModell.class );

      final LinearSumBean bean = LinearSumBean.createFromModel( model );
      bean.setProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE, parameterType );
      return bean;
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      // If this happens, it's an internal bug!
      return null;
    }
  }

  public static void showWizard( final Shell shell, final LinearSumBean bean, final ITreeNodeModel model, final String windowTitle )
  {
    try
    {
      /* Init timeseries gml */
      final ICoreRunnableWithProgress operation = new InitThiessenTimeseriesOperation( bean );
      final IStatus initStatus = ProgressUtilities.busyCursorWhile( operation );
      if( !initStatus.isOK() )
      {
        StatusDialog.open( shell, initStatus, windowTitle );
        return;
      }

      final Wizard wizard = new ThiessenGeneratorWizard( bean );
      wizard.setWindowTitle( windowTitle );

      final WizardDialog2 dialog = new WizardDialog2( shell, wizard );
      dialog.setRememberSize( true );
      if( dialog.open() != Window.OK )
        return;

      try
      {
        /* Apply the changes. */
        final Feature generator = bean.apply( model.getWorkspace(), (String) bean.getProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE ) );

        /* Refresh the tree. */
        model.refreshTree( generator );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to save the model", e ); //$NON-NLS-1$
        StatusDialog.open( shell, status, shell.getText() );
      }
    }
    finally
    {
      /* Delete the generated stations gml. */
      deleteStationsGmlQuietly();
    }
  }

  /**
   * This function deletes the generated stations gml. It will not throw an exception on failure. The stations gml is
   * purly temporary and will be overwritten the next time the wizard opens if it was not deleted.
   */
  public static void deleteStationsGmlQuietly( )
  {
    try
    {
      final SzenarioDataProvider scenarioDataProvider = ScenarioHelper.getScenarioDataProvider();
      final IContainer scenarioFolder = scenarioDataProvider.getScenarioFolder();
      final IFile thiessenFile = scenarioFolder.getFile( new Path( INaProjectConstants.GML_THIESSEN_STATION_PATH ) );
      thiessenFile.delete( true, new NullProgressMonitor() );
    }
    catch( final Exception ex )
    {
      // Ignore
      ex.printStackTrace();
    }
  }

  public static IThiessenStationCollection loadThiessenStations( ) throws CoreException
  {
    try
    {
      /* timeseries file */
      final SzenarioDataProvider scenarioDataProvider = ScenarioHelper.getScenarioDataProvider();
      final IContainer scenarioFolder = scenarioDataProvider.getScenarioFolder();
      final IFile thiessenFile = scenarioFolder.getFile( new Path( INaProjectConstants.GML_THIESSEN_STATION_PATH ) );

      /* load file and transform to kalypso crs */
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( thiessenFile );
      workspace.accept( new TransformVisitor( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() ), FeatureVisitor.DEPTH_INFINITE );
      return (IThiessenStationCollection) workspace.getRootFeature();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "TimeseriesThiessenPolygons_1" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }
}