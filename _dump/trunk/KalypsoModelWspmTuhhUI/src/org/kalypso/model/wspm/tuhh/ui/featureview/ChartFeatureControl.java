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
package org.kalypso.model.wspm.tuhh.ui.featureview;

import java.net.URL;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ogc.gml.featureview.control.IFeatureControl;
import org.kalypso.swtchart.chart.Chart;
import org.kalypso.swtchart.chart.ChartUtilities;
import org.kalypso.swtchart.configuration.ChartLoader;
import org.kalypsodeegree.model.feature.Feature;
import org.ksp.chart.configuration.ChartType;

/**
 * @author Gernot Belger
 */
public class ChartFeatureControl extends AbstractFeatureControl implements IFeatureControl
{
  private Chart[] m_charts;
  private final ChartType[] m_chartTypes;
  private final URL m_context;

  public ChartFeatureControl( final Feature feature, final IPropertyType ftp, final ChartType[] charts, final URL context )
  {
    super( feature, ftp );
    
    m_chartTypes = charts;
    m_context = context;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    final TabFolder folder = new TabFolder( parent, SWT.TOP );
    
    m_charts = new Chart[m_chartTypes.length];
    for( int i = 0; i < m_chartTypes.length; i++ )
    {
      final TabItem item = new TabItem( folder, SWT.NONE );
      
      final ChartType chartType = m_chartTypes[i];
      item.setText( chartType.getTitle() );
      item.setToolTipText( chartType.getDescription() );
      
      m_charts[i] = new Chart( folder, style );
      item.setControl( m_charts[i] );
    }
    
    updateControl();
    
    return folder;
  }
  
  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#isValid()
   */
  public boolean isValid( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    // TODO Auto-generated method stub
  }
  
  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
    for( int i = 0; i < m_charts.length; i++ )
    {
      final Chart chart = m_charts[i];
      final ChartType chartType = m_chartTypes[i];
      
      ChartLoader.configureChart( chart, chartType, m_context );
      ChartUtilities.maximize( chart );
    }
  }

}
