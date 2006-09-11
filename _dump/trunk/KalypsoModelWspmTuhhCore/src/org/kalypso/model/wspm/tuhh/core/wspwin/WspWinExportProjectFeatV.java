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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.io.File;
import java.net.URL;
import java.util.Iterator;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author thuel2
 */
public class WspWinExportProjectFeatV implements FeatureVisitor, IWspmTuhhConstants
{
  public static IStatus exportWspmProject( Iterator modelGml, File wspwinDir, SubProgressMonitor monitor )
  {
    // modelGml holds the selected resources (very general)
    while( modelGml.hasNext() )
    {
      // TODO: ensure that they are TU-HH models (and GML files)
      try
      {
        final IResource resource = (IResource) modelGml.next();
        if( resource instanceof IFile )
        {
          // read gml workspace
          final URL modelGmlUrl = ResourceUtilities.createURL( resource );

          final GMLWorkspace gmlWrkSpce = GmlSerializer.createGMLWorkspace( modelGmlUrl, null );

          final WspWinExportProjectFeatV exportVisitor = new WspWinExportProjectFeatV( modelGml, wspwinDir, monitor );
          gmlWrkSpce.accept( exportVisitor, gmlWrkSpce.getRootFeature(), FeatureVisitor.DEPTH_INFINITE_LINKS );
        }
        else
          // we don't read folders
          continue;
      }
      catch( final Exception e1 )
      {
        e1.printStackTrace();
        return StatusUtilities.statusFromThrowable( e1 );
      }
    }
    return Status.OK_STATUS;
  }

  private static File m_wspwinDir;

  private static SubProgressMonitor m_monitor;

  private MultiStatus m_status = new MultiStatus( PluginUtilities.id( KalypsoModelWspmTuhhCorePlugin.getDefault() ), 0, "", null );

  private final Iterator m_modelGml;

  public WspWinExportProjectFeatV( Iterator modelGml, File wspwinDir, SubProgressMonitor monitor )
  {
    m_modelGml = modelGml;
    m_wspwinDir = wspwinDir;
    m_monitor = monitor;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( Feature f )
  {
    // featType holen
    final IFeatureType featureType = f.getFeatureType();
    final QName featureName = featureType.getQName();

    if( QNameUtilities.equals( featureName, IWspmConstants.NS_WSPMPROJ, "WspmProject" ) )
    {
      return true;
    }
    if( QNameUtilities.equals( featureName, IWspmConstants.NS_WSPM, "WaterBody" ) )
    {
      return true;
    }
    //
    if( QNameUtilities.equals( featureName, NS_WSPM_TUHH, "CalculationWspmTuhhSteadyState" ) )
    {
      return true;
    }

    //
    if( QNameUtilities.equals( featureName, NS_WSPM_TUHH, "ReachWspmTuhhSteadyState" ) )
    // TODO: Besucher losschicken...
    {
      System.out.println( "Juhuu" );
      return false;
    }
    return true;

    // m_status.add( StatusUtilities.statusFromThrowable( e ) );
  }

  public IStatus getStatus( )
  {
    return m_status;
  }
}
