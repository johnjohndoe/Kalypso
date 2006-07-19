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
package org.kalypso.ogc.gml.om.chart;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.swtchart.Chart;
import org.kalypso.swtchart.axis.IAxis;
import org.kalypso.swtchart.configuration.StyleLoader;
import org.kalypso.swtchart.layer.IChartLayer;
import org.kalypso.swtchart.layer.ILayerProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.ksp.chart.layerstyle.StyleType;
import org.ksp.chart.viewerconfiguration.AbstractAxisType;
import org.ksp.chart.viewerconfiguration.ChartElementType;
import org.ksp.chart.viewerconfiguration.DataSourceType;
import org.ksp.chart.viewerconfiguration.LayerProviderType;
import org.ksp.chart.viewerconfiguration.ChartElementType.AxisMapping;

/**
 * @author alibu
 *
 */
public class WQTabelleLayerProvider implements ILayerProvider
{

  private final Chart m_chart;
  private final LayerProviderType m_lp;

  public WQTabelleLayerProvider( LayerProviderType lp, Chart chart )
  {
    m_lp = lp;
    m_chart = chart;
      
  }

  /**
   * @see org.kalypso.swtchart.layer.ILayerProvider#getLayers()
   */
  public IChartLayer[] getLayers( )
  {
IChartLayer[] icl=new IChartLayer[1];
    
    List<DataSourceType> dataSource = m_lp.getDataSource();
    for( DataSourceType ds : dataSource )
    {
      String url=ds.getUrl();
      Object o=null;
        
        try
        {
          GMLWorkspace workspace = GmlSerializer.createGMLWorkspace(new URL(url));
          Feature feature = workspace.getFeature("wq_table1");
          if (feature!=null)
          {
            System.out.println("Found feature: "+feature.getId());
          }
          IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation(feature);
          
          List<ChartElementType> ces = m_lp.getChartElement();
          for( ChartElementType ce : ces )
          {
             AxisMapping am = ce.getAxisMapping();
             
             AbstractAxisType domLink = (AbstractAxisType) am.getDomain().getAxisLink();
             IAxis domAxis=m_chart.getAxisRegistry().getAxis(domLink.getId());

             AbstractAxisType valLink = (AbstractAxisType) am.getValue().getAxisLink();
             IAxis valAxis=m_chart.getAxisRegistry().getAxis(valLink.getId());
             
             TupleResult result = obs.getResult();
             
             //ResultComponent rausfinden
             final IComponent[] comps = result.getComponents();
             final TreeMap<String, IComponent> map = new TreeMap<String, IComponent>(  );
             for( int i = 0; i < comps.length; i++ )
             {
               final QName qn = comps[i].getValueTypeName();

               if (comps[i].getName().compareTo("Wasserstand")==0)
                   map.put("domain", comps[i] );
               else if (comps[i].getName().compareTo("Abfluss")==0)
                 map.put("value", comps[i] );
               System.out.println("Component: "+comps[i].getName());
               System.out.println("Component: "+comps[i].getDescription());
               System.out.println("Component: "+comps[i].getPosition());
               System.out.println("Component: "+comps[i].getValueTypeName());
               if( map.size() == 2 )
                 break;
             }
             
             TupleResultLineChartLayer trcl=new TupleResultLineChartLayer(result, map.get("domain"),map.get("value") , domAxis, valAxis);
             trcl.setStyle(StyleLoader.createStyle((StyleType) ce.getStyleLink().getRef()));
             icl[0]=trcl;
          }
        }
        catch( MalformedURLException e )
        {
          e.printStackTrace();
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
        return icl;
    }
    return null;
  }

}
