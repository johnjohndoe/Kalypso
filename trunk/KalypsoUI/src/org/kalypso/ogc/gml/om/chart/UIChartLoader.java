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
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;

import org.eclipse.swt.graphics.Color;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.swtchart.Chart;
import org.kalypso.swtchart.axis.AxisFactory;
import org.kalypso.swtchart.axis.renderer.DateAxisRenderer;
import org.kalypso.swtchart.axis.renderer.IAxisRenderer;
import org.kalypso.swtchart.axis.renderer.NumberAxisRenderer;
import org.kalypso.swtchart.layer.IChartLayer;
import org.kalypso.swtchart.layer.impl.DoubleDataLayerProvider;
import org.kalypso.swtchart.styles.ILayerStyle;
import org.kalypso.swtchart.styles.LayerStyle;
import org.kalypso.swtchart.styles.StyleHelper;
import org.kalypso.swtchart.styles.StyledLine;
import org.kalypso.swtchart.styles.StyledPoint;
import org.kalypso.swtchart.styles.StyledPolygon;
import org.kalypso.swtchart.styles.IStyleConstants.SE_LINESTYLE;
import org.ksp.chart.layerstyle.AbstractPointType;
import org.ksp.chart.layerstyle.ImagePointType;
import org.ksp.chart.layerstyle.LineType;
import org.ksp.chart.layerstyle.PointType;
import org.ksp.chart.layerstyle.PolygonType;
import org.ksp.chart.layerstyle.StyleType;
import org.ksp.chart.viewerconfiguration.AbstractAxisType;
import org.ksp.chart.viewerconfiguration.ChartType;
import org.ksp.chart.viewerconfiguration.ConfigurationType;
import org.ksp.chart.viewerconfiguration.DataSourceType;
import org.ksp.chart.viewerconfiguration.LayerProviderType;
import org.ksp.chart.viewerconfiguration.ObjectFactory;


/**
 * @author alibu
 *
 */
public class UIChartLoader extends org.kalypso.swtchart.configuration.ChartLoader
{
  

  public UIChartLoader( Chart chart, String path )
  {
     super(chart, path);
  }

  /**
   * Erstellt ein Chart aus der Konfiguration  
   * @param chart Das Chart-Objekt, das mit Leben gefüllt wird
   * @param configChartTitle Der Titel der ChartConfiguration, die verwendet werden soll
   */
  public Chart createChart(String configChartTitle)
  {
    ChartType configChart=null;
    
    //ChartConfig auslesen
    if (m_config!=null)
    {
      List<ChartType> ccharts = m_config.getChart();
      for( ChartType c : ccharts )
      {
          if (c.getTitle().compareTo(configChartTitle)==0)
          {
            configChart=c;
            break;
          }
      }
    }
    

    
    
    if (configChart!=null)
    {
      
        //Achsen hinzufügen
      
        List<JAXBElement< ? extends AbstractAxisType>> jaxbAxes = configChart.getAxis();
        for( JAXBElement< ? extends AbstractAxisType> element : jaxbAxes )
        {
            AbstractAxisType abstractAxis=element.getValue();
            AxisFactory.addConfigAxis(m_chart, abstractAxis);
        }
        
        
        //Layer hinzufügen
        List<LayerProviderType> lps = configChart.getLayerProvider();
        for( LayerProviderType lp : lps )
        {
            String lpname=lp.getName();
            if (lpname.compareTo("org.ksp.observation.layerProvider.Wasserstand")==0)
            {
               System.out.println("LayerProvider: Wasserstand");
               WasserstandLayerProvider wlp=new WasserstandLayerProvider(lp, m_chart); 
               IChartLayer[] icl=wlp.getLayers();
               for( IChartLayer layer : icl )
               {
                 m_chart.addLayer(layer);
               }
               //DefaultTupleResultLayerProvider lp=new DefaultTupleResultLayerProvider(chart.getAxisRegistry(), obs, domainComp, valueComp, domAxis, valAxis);
            }
            else if (lpname.compareTo("org.ksp.observation.layerProvider.WQTabelle")==0)
            {
              System.out.println("LayerProvider: WQTabelle");
              WQTabelleLayerProvider wqlp=new WQTabelleLayerProvider(lp, m_chart); 
              IChartLayer[] icl=wqlp.getLayers();
              for( IChartLayer layer : icl )
              {
                m_chart.addLayer(layer);
              }
            }
            else if (lpname.compareTo("org.ksp.observation.layerProvider.Niederschlag")==0)
            {
              System.out.println("LayerProvider: Niederschlag");
              NiederschlagLayerProvider wqlp=new NiederschlagLayerProvider(lp, m_chart); 
              IChartLayer[] icl=wqlp.getLayers();
              for( IChartLayer layer : icl )
              {
                m_chart.addLayer(layer);
              }
            }
            else if (lpname.compareTo("org.ksp.observation.layerProvider.Alarmstufe")==0)
            {
              System.out.println("LayerProvider: Alarmstufen");
            }
            else
            {
              super.createChart(configChartTitle);
            }
            
        }
        
        
    }
    return m_chart;
  }
 


  
}

  