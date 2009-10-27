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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintWriter;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IExportWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author kimwerner
 */

public class LengthSectionExportWizard extends Wizard implements IExportWizard
{
  private IObservation<TupleResult> m_observation = null;

  private LengthSectionExportPage m_lengthSectionExportPage;

  /**
   * Creates a wizard for exporting workspace resources into a wspwin project.
   */
  public LengthSectionExportWizard( )
  {
    final IDialogSettings pluginSettings = KalypsoModelWspmTuhhUIPlugin.getDefault().getDialogSettings();
    final IDialogSettings section = pluginSettings.getSection( "LengthSectionExportWizard" );//$NON-NLS-1$
    if( section != null )
      setDialogSettings( section );
    else
      setDialogSettings( pluginSettings.addNewSection( "WspWinExportWizard" ) );//$NON-NLS-1$

    setForcePreviousAndNextButtons( false );
  }

  /*
   * (non-Javadoc) Method declared on IWizard.
   */
  @Override
  public void addPages( )
  {
    m_lengthSectionExportPage = new LengthSectionExportPage( "test" );
    addPage( m_lengthSectionExportPage );
  }

  /*
   * (non-Javadoc) Method declared on IWorkbenchWizard.
   */
  public void init( final IWorkbench workbench, final IStructuredSelection currentSelection )
  {
    try
    {
      if( !currentSelection.isEmpty() )
      {
        Object firstElement = currentSelection.getFirstElement();
        if( firstElement instanceof IFile )
        {
          IFile file = (IFile) firstElement;
          GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( file );
          Feature rootFeature = workspace.getRootFeature();
          m_observation = ObservationFeatureFactory.toObservation( rootFeature );
          workspace.dispose();
        }
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    setWindowTitle( Messages.getString( "Length Section Export" ) );
    setNeedsProgressMonitor( true );
  }

  /*
   * (non-Javadoc) Method declared on IWizard.
   */
  @Override
  public boolean performFinish( )
  {
    try
    {
      if( m_lengthSectionExportPage.OpenPlotter() )
      {
        final File file = new File( System.getProperty( "java.io.tmpdir" ), "exportTmp.lng" );//$NON-NLS-1$ //$NON-NLS-2$
        final IProfilSink sink = KalypsoModelWspmCoreExtensions.createProfilSink( "lng" );
        sink.write( m_observation, new PrintWriter( new FileOutputStream( file ) ) );
        KalypsoCorePlugin.getDefault().getPreferenceStore().setValue( IWspmConstants.WSPWIN_PLOTTER_PATH, m_lengthSectionExportPage.getText() );
        Runtime.getRuntime().exec( "\"" + m_lengthSectionExportPage.getText() + "\" \"" + file.getPath() + "\"" );//$NON-NLS-1$ //$NON-NLS-2$// $NON-NLS-3$
      }
      else
      {
        final File file = new File( m_lengthSectionExportPage.getText() );
        final IProfilSink sink = KalypsoModelWspmCoreExtensions.createProfilSink( "lng" );
        sink.write( m_observation, new PrintWriter( new FileOutputStream( file ) ) );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    return true;
  }

}
