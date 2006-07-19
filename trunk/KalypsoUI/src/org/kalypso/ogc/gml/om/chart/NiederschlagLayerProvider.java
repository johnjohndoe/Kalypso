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

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;



import ogc31.www.opengis.net.ogc.ObjectFactory;

import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.gmlschema.feature.FeatureType;
import org.kalypso.jwsdp.JaxbUtilities;
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
public class NiederschlagLayerProvider implements ILayerProvider
{
  
  
  private LayerProviderType m_lpt;
  private Chart m_chart;

  public NiederschlagLayerProvider(LayerProviderType lpt, Chart chart)
  {
    m_lpt=lpt;
    m_chart=chart;
    
  }
    
  /**
   * @see org.kalypso.swtchart.layer.ILayerProvider#getLayers()
   */
  public IChartLayer[] getLayers( )
  {
    IChartLayer[] icl=new IChartLayer[1];
    
    List<DataSourceType> dataSource = m_lpt.getDataSource();
    for( DataSourceType ds : dataSource )
    {
      String url=ds.getUrl();
      Object o=null;
        
        try
        {
          GMLWorkspace workspace = GmlSerializer.createGMLWorkspace(new URL(url));
          Feature feature = workspace.getFeature("niederschlagsmessung");
          if (feature!=null)
          {
            System.out.println("Found feature: "+feature.getId());
          }
          IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation(feature);
          
          List<ChartElementType> ces = m_lpt.getChartElement();
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
               /*
                * TODO: evtl. ist es nicht klug, die Name-Variable zu verwenden;
                * besser: id, aber das wird nicht von der Componente zur¸ckgegebn
                */
               IComponent comp=comps[i];
               System.out.println("Component: "+comp.getName());
               System.out.println("Component: "+comp.getDescription());
               System.out.println("Component: "+comp.getPosition());
               System.out.println("Component: "+comp.getValueTypeName());
               if (comp.getName().compareTo("Datum")==0)
                 map.put("domain", comp );
               else if (comp.getName().compareTo("Niederschlag")==0)
                 map.put("value", comp );
               if( map.size() == 2 )
                 break;
             }
             
             TupleResultBarChartLayer trcl=new TupleResultBarChartLayer(result, map.get("domain"),map.get("value") , domAxis, valAxis);
             trcl.setStyle(StyleLoader.createStyle((StyleType) ce.getStyleLink().getRef()));
             
             icl[0]=trcl;
          }
          
          
          
        }
        catch( MalformedURLException e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
        catch( Exception e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
        return icl;
    }
    return null;
  }

}
