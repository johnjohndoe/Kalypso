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
package org.kalypso.ui.model.wspm.core.wspwin;

import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.text.DateFormatter;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.model.wspm.KalypsoUIModelWspmPlugin;
import org.kalypso.ui.model.wspm.abstraction.TuhhReach;
import org.kalypso.ui.model.wspm.abstraction.TuhhWspmProject;
import org.kalypso.ui.model.wspm.abstraction.WspmProject;
import org.kalypso.ui.model.wspm.core.wspwin.WspCfgBean.ZustandBean;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author thuel2
 */
/**
 * @author thuel2
 */
public class WspWinImporter
{
  public static final String DIR_PROF = "prof";

  public static final String FILE_PROBEZ = "probez.txt";

  private static final DateFormat DATE_FORMATTER = SimpleDateFormat.getDateTimeInstance( SimpleDateFormat.SHORT, SimpleDateFormat.SHORT );

  private WspWinImporter( )
  {
    // will not be instantiated
  }

  /**
   * Imports a wpswin project into a folder or a project.
   * <p>
   * Prerequisite: container holds a valid wspm-tuhh structure
   * </p>
   */
  public static void importProject( final File wspwinDirectory, final IContainer targetContainer, final IProgressMonitor monitor ) throws Exception
  {
    // HACK: initialize KalypsoUI
    KalypsoGisPlugin.getDefault();

    monitor.beginTask( "WspWin Projekt importieren", 1000 );

    try
    {
      // load gml workspace
      monitor.subTask( "Modell wird geladen..." );
      final IFile modelFile = targetContainer.getFile( new Path( "wspmTuhhModel.gml" ) );
      final URL url = ResourceUtilities.createURL( modelFile );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( url );
      monitor.worked( 200 );

      monitor.subTask( "WspWin Projekt wird geladen..." );
      // load wspwin data

      monitor.subTask( "Daten werden konvertiert..." );
      // fill wspwin data into workspace
      final Feature modelRootFeature = workspace.getRootFeature();
//      final IFeatureType modelRootFT = modelRootFeature.getFeatureType();

      final WspmProject wspmProject = new WspmProject( modelRootFeature );
      
      // ////////////// //
      // set model name //
      // ////////////// //
      final String oldProjectName = wspmProject.getName();
      final String modelName = oldProjectName + wspwinDirectory.getName();
      wspmProject.setName( modelName );

      // ///////////////////// //
      // set model description //
      // ///////////////////// //
      final String oldProjectDescription = wspmProject.getDescription();
      final StringBuffer modelDescription = new StringBuffer( oldProjectDescription == null ? "" : oldProjectDescription );
      if( modelDescription.length() != 0 )
        modelDescription.append( "\n\n" );

      modelDescription.append( "Projekt wurde aus WspWin Projekt " );
      modelDescription.append( wspwinDirectory.getAbsolutePath() );
      modelDescription.append( " importiert (" );
      modelDescription.append( DATE_FORMATTER.format( new Date( System.currentTimeMillis() ) ) );
      modelDescription.append( ").");
      
      final String wspwinModelDescription = readProjectDescription( wspwinDirectory );
      if( wspwinModelDescription != null )
      {
        modelDescription.append( "\nWspWin Projektbeschreibung: " );
        modelDescription.append( wspwinModelDescription );
      }

      wspmProject.setDescription( modelDescription.toString() );

      // /////////// //
      // add reaches //
      // /////////// //
      
      // load wsp.cfg
      final WspCfgBean wspCfgBean = WspCfgBean.read( wspwinDirectory );
      if( wspCfgBean.getType() != 'b' )
      {
        PluginUtilities.logToPlugin( KalypsoUIModelWspmPlugin.getDefault(), IStatus.WARNING, "Es wird ein WspWin-Knauf Projekt als TUHH-Pasche-Projekt importiert.", null );
        wspCfgBean.setType( 'b' );
      }
      
      // TODO: first import profiles from profproj.txt
      
      // from now on, we have tuhh projects: if we later support other kinds of projects, tewak here
      final TuhhWspmProject tuhhProject = new TuhhWspmProject( modelRootFeature );
      final ZustandBean[] zustaende = wspCfgBean.getZustaende();
      // foreach make a reach
      for( final ZustandBean zustandBean : zustaende )
        importTuhhZustand( tuhhProject, zustandBean );
      
      // add them whith new id's

      // /////////////// //
      // write workspace //
      // ////////////// //
      monitor.subTask( "Modell wird geschrieben..." );
      final SetContentHelper contentHelper = new SetContentHelper()
      {
        @Override
        protected void write( final OutputStreamWriter writer ) throws Throwable
        {
          GmlSerializer.serializeWorkspace( writer, workspace );
        }
      };

      contentHelper.setFileContents( modelFile, false, true, new SubProgressMonitor( monitor, 200 ) );
    }
    finally
    {
      monitor.done();
    }
  }

  private static void importTuhhZustand( final TuhhWspmProject tuhhProject, final ZustandBean zustandBean )
  {
    final String name = zustandBean.getName();
    System.out.println( "Importing "  + name );
    final TuhhReach reach = tuhhProject.createNewReach( zustandBean.getWaterName() );
    reach.setName( name );
    
    final StringBuffer descBuffer = new StringBuffer();
    descBuffer.append( "Imported from WspWin\n" );
    descBuffer.append( "Originally created: " + DATE_FORMATTER.format( zustandBean.getDate() ) );
    
    reach.setDescription( descBuffer.toString() );
    
    // add reachSegments + profiles (.str)
    
    // add runoff events (.wsf, .qwt)
    
    // add calculations (.ber, .001)
    
  }

  /** Returns the content of the prof/probez.txt file */
  public static String readProjectDescription( final File wspwinDirectory ) throws IOException
  {
    final File probezFile = new File( getProfDir( wspwinDirectory ), FILE_PROBEZ );

    return FileUtils.readFileToString( probezFile, null );
  }

  public static File getProfDir( final File wspwinDirectory )
  {
    return new File( wspwinDirectory, DIR_PROF );
  }

}
