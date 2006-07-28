/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ogc.gml.om.chart;

import java.awt.Insets;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.views.navigator.LocalSelectionTransfer;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.AbstractObservationView;
import org.kalypso.swtchart.chart.Chart;
import org.kalypso.swtchart.chart.Diagram;
import org.kalypso.swtchart.configuration.ChartLoader;
import org.kalypso.swtchart.configuration.ConfigurationLoader;

/**
 * @author schlienger
 */
public class ObservationChartView extends AbstractObservationView
{
  private final static int AXIS_WIDTH = 1;

  private final static int TICK_LENGTH = 10;

  private static final Insets LABEL_INSETS = new Insets( 5, 0, 5, 0 );

  private static final Insets TICK_INSETS = new Insets( 2, 10, 10, 10 );


 // private Composite m_composite;

  private Diagram m_diagram;

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_diagram != null )
      m_diagram.dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( Composite parent )
  {
    /*
    m_composite = new Composite( parent, SWT.FILL );
    FillLayout fillLayout = new FillLayout(   ) ;
    fillLayout.type = SWT.VERTICAL;
    m_composite.setLayout( fillLayout);
*/
    m_diagram=new Diagram(parent, SWT.FILL);
    //m_diagram.setLayoutData(new FillData());
    
    addDropSupport( DND.DROP_COPY | DND.DROP_MOVE | DND.DROP_LINK, new Transfer[] { LocalSelectionTransfer.getInstance() } );
    

   // m_composite.layout();
    
    
    /*
    final Color cfg = parent.getDisplay().getSystemColor( SWT.COLOR_DARK_GRAY );
    final Color cbg = parent.getDisplay().getSystemColor( SWT.COLOR_WHITE );

    
    final NumberAxisRenderer numberAxisRenderer = new NumberAxisRenderer( cfg, cbg, AXIS_WIDTH, TICK_LENGTH, TICK_INSETS, 1, LABEL_INSETS, 4 );
    final IAxisRegistry axisRegistry = m_chart.getAxisRegistry();
    axisRegistry.setRenderer( Number.class, numberAxisRenderer );

    final DoubleComparator dc = new DoubleComparator( 0.001 );
    final NumberAxis botAxis = new NumberAxis( "1", "domain", PROPERTY.CONTINUOUS, POSITION.BOTTOM, DIRECTION.POSITIVE, dc );
    final NumberAxis leftAxis = new NumberAxis( "2", "value", PROPERTY.CONTINUOUS, POSITION.LEFT, DIRECTION.POSITIVE, dc );

    axisRegistry.addAxis( botAxis );
    axisRegistry.addAxis( leftAxis );

    */
    // testing
   // m_chart.addLayer( new TestLayer( botAxis, leftAxis ) );
    
    
   //
    
    
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    ((Composite) m_diagram).setFocus();
  }

  /**
   * @see org.kalypso.ogc.gml.om.views.AbstractObservationView#getControl()
   */
  @Override
  protected Control getControl( )
  {
    return m_diagram;
  }

  /**
   * @see org.kalypso.ogc.gml.om.views.AbstractObservationView#handleDrop(org.kalypso.observation.IObservation)
   * Das ist der Drag & Drop Mechanisimus, der zur zeit aber keine Aktion ausführt. Hier muss noch ein Konzept her,
   * wie die einzelnen Observation-Results den LayerProvidern (oder Layern) zugeordnet werden sollen; am besten vielleicht
   * per Configurationsdatei
   */
  @Override
  protected boolean handleDrop( final IObservation<TupleResult> obs )
  { 
    System.out.println("Something has been dropped");
    final TupleResult result = obs.getResult();
    final IComponent[] comps = result.getComponents();
    final List<IComponent> list = new ArrayList<IComponent>( 2 );
    for( int i = 0; i < comps.length; i++ )
    {
      final QName qn = comps[i].getValueTypeName();

      if( XmlTypes.isNumber( qn ) /* || XmlTypes.isDate( qn ) */)
        list.add( comps[i] );

      if( list.size() == 2 )
        break;
    }

    if( list.size() < 2 )
      return false;

    final IComponent domComp = list.get( 0 );
    final IComponent valComp = list.get( 1 );
    
    /*
    final DefaultTupleResultLayerProvider layerProvider = new DefaultTupleResultLayerProvider( m_chart.getAxisRegistry(), obs, domComp, valComp, "1", "2" );

    final IChartLayer[] layers = layerProvider.getLayers();

    for( int i = 0; i < layers.length; i++ )
    {
      final IChartLayer layer = layers[i];
      System.out.println( "Would like to add layer: " + layer );
      m_chart.addLayer( layer );
    }
    */
    return true;
  }
}
