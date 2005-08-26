/*
 * --------------- Kalypso-Header --------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml.featureview.dialog;

import java.util.Collection;

import javax.xml.bind.JAXBException;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.sensor.view.ObservationViewerDialog;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * TimeserieLinkFeatureDialog
 * 
 * @author schlienger (23.05.2005)
 */
public class TimeserieLinkFeatureDialog implements IFeatureDialog
{
  private final Feature m_feature;

  private final FeatureTypeProperty m_ftp;

  private FeatureChange m_change;

  private final GMLWorkspace m_workspace;

  public TimeserieLinkFeatureDialog( final GMLWorkspace workspace, final Feature feature, final FeatureTypeProperty ftp )
  {
    m_workspace = workspace;
    m_feature = feature;
    m_ftp = ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#open(org.eclipse.swt.widgets.Shell)
   */
  public int open( final Shell shell )
  {
    ObservationViewerDialog dialog = new ObservationViewerDialog( shell );
    final TimeseriesLink tslink = (TimeseriesLink)m_feature.getProperty( m_ftp.getName() );
    dialog.setContext( m_workspace.getContext() );
    dialog.setInput( tslink == null ? "" : tslink.getHref() );

    final int open = dialog.open();
    FeatureChange fChange = null;
    if( open == Window.OK )
    {
      final String href = (String)dialog.getInput();
      if( href == null )
        fChange = new FeatureChange( m_feature, m_ftp.getName(), null );
      else if( href != null && href.length() > 0 )
      {
        final ObjectFactory linkFactory = new ObjectFactory();
        try
        {
          final TimeseriesLink link = linkFactory.createTimeseriesLink();
          link.setHref( href );
          fChange = new FeatureChange( m_feature, m_ftp.getName(), link );
        }
        catch( JAXBException e )
        {
          e.printStackTrace();
        }
      }
    }
    m_change = fChange;
    return open;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#collectChanges(java.util.Collection)
   */
  public void collectChanges( final Collection c )
  {
    if( m_change != null )
      c.add( m_change );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#getLabel()
   */
  public String getLabel()
  {
    return "...";
  }
}
