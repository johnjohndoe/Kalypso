/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
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
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml.featureview.dialog;

import java.util.Collection;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.view.ObservationViewerDialog;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author kuepfer
 */
public class ZmlInlineFeatureDialog implements IFeatureDialog
{

  private GMLWorkspace m_workspace;

  private Feature m_feature;

  private FeatureTypeProperty m_ftp;

  private FeatureChange m_change;

  public ZmlInlineFeatureDialog( final GMLWorkspace workspace, final Feature feature, final FeatureTypeProperty ftp )
  {
    m_workspace = workspace;
    m_feature = feature;
    m_ftp = ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#open(org.eclipse.swt.widgets.Shell)
   */
  public int open( Shell shell )
  {
    final ObservationViewerDialog dialog = new ObservationViewerDialog( shell, false, true, true );
    Object o = m_feature.getProperty( m_ftp.getName() );
    if( o instanceof IObservation )
      dialog.setObservation( (IObservation)o );
    int open = dialog.open();
    FeatureChange fchange = null;
    if( open == Window.OK )
    {
      fchange = new FeatureChange( m_feature, m_ftp.getName(), o );
      m_change = fchange;
    }
    return open;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#collectChanges(java.util.Collection)
   */
  public void collectChanges( Collection c )
  {
    if( m_change != null )
      c.add( m_change );

  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#getLabel()
   */
  public String getLabel()
  {
    return "X-Y Diagram...";
  }

}
