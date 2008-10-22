/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.ant;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.ogc.util.MapZmlMeta2FeatureVisitor;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * Reads data from a zml (linked into the visited features) and puts it as property into the same feature.
 * 
 * @see org.kalypso.ogc.util.MapZmlMeta2FeatureVisitor
 * 
 * @author belger
 */
public class MapZmlMeta2FeatureTask extends AbstractFeatureVisitorTask
{
  /** FeatureProperty which holds the Zml-Link */
  private String m_zmlLink;
  
  /** List of mapoings to perform */
  private List<MapZmlMeta2FeatureVisitor.Mapping> m_mappings = new ArrayList<MapZmlMeta2FeatureVisitor.Mapping>( 5 );

  public MapZmlMeta2FeatureTask(  )
  {
    super( true );
  }
  
  public void setZmlLink( final String zmlLink )
  {
    m_zmlLink = zmlLink;
  }
  
  public void addConfiguredMapping( final MapZmlMeta2FeatureVisitor.Mapping mapping )
  {
    m_mappings.add( mapping ); 
  }
  
  /**
   * @see org.kalypso.ant.AbstractFeatureVisitorTask#createVisitor(java.net.URL, org.kalypso.contribs.java.net.IUrlResolver, org.kalypso.contribs.java.util.logging.ILogger, org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected FeatureVisitor createVisitor( final URL context, final IUrlResolver resolver,  final ILogger logger,
      final IProgressMonitor monitor )
  {
    return new MapZmlMeta2FeatureVisitor( context, resolver, m_zmlLink, m_mappings.toArray( new MapZmlMeta2FeatureVisitor.Mapping[m_mappings.size()] ) );
  }

  /**
   * @see org.kalypso.ant.AbstractFeatureVisitorTask#validateInput()
   */
  @Override
  protected void validateInput()
  {
  // nothing to validate
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.IErrorHandler#handleError(org.eclipse.swt.widgets.Shell,
   *      org.eclipse.core.runtime.IStatus)
   */
  public void handleError( final Shell shell, final IStatus status )
  {
    ErrorDialog.openError(shell, "MapZmlMeta2Feature", "Fehler beim Erzeugen der Feature-Properties.", status );
  }
}
