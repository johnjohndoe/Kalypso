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


import java.util.ArrayList;
import java.util.GregorianCalendar;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.collections.comparators.ComparableComparator;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Point;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.swtchart.axis.IAxis;
import org.kalypso.swtchart.axis.IAxisConstants.ORIENTATION;
import org.kalypso.swtchart.axis.IAxisConstants.POSITION;
import org.kalypso.swtchart.layer.IChartLayer;
import org.kalypso.swtchart.layer.IDataRange;
import org.kalypso.swtchart.layer.impl.DataRange;
import org.kalypso.swtchart.legend.ILegendItem;
import org.kalypso.swtchart.styles.ILayerStyle;
import org.kalypso.swtchart.styles.StyledLine;
import org.kalypso.swtchart.styles.StyledPoint;
import org.kalypso.swtchart.styles.StyledPolygon;
import org.kalypso.swtchart.styles.IStyleConstants.SE_TYPE;
import org.kalypso.swtchart.util.AliComparator;
import org.kalypso.swtchart.util.Recalc;

import sun.util.calendar.Gregorian;

/**
 * @author schlienger
 */
public class TupleResultBarChartLayer implements IChartLayer
{
  private final TupleResult m_result;

  private final IAxis m_valAxis;

  private final IComponent m_domComp;

  private final IComponent m_valComp;

  private final IAxis m_domAxis;

  private ILayerStyle m_style;

  public TupleResultBarChartLayer( TupleResult result, IComponent domComp, IComponent valComp, IAxis domAxis, IAxis valAxis )
  {
    m_result = result;
    m_domComp = domComp;
    m_valComp = valComp;
    m_domAxis = domAxis;
    m_valAxis = valAxis;
  }

  /**
   * @see org.kalypso.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Device)
   */
  public void paint( final GCWrapper gc, final Device dev )
  {
    int size = m_result.size();
    gc.setLineWidth(5);
   // gc.drawLine(0,0, 200, 200);
    System.out.println("Drawing TupleResultChartLayer");
    
    StyledPolygon sp=(StyledPolygon) getStyle().getElement(SE_TYPE.POLYGON, 0);
    
    //Intervall: ein Tag in Millisekunden
    double barInterval=24*60*60*1000;
    //Fixpunkt für Intervall setzten: Hauptsache 0 Uhr
    GregorianCalendar fixedGregCal=new GregorianCalendar();
    fixedGregCal.set(2000, 1, 1, 0,0);
    XMLGregorianCalendar fixedPoint=Recalc.gregCalToXMLGregCal(fixedGregCal);
    
    
    for( int i = 0; i < size; i++)
    {
      final IRecord r1 = m_result.get( i );

      Object domVal = m_result.getValue( r1, m_domComp );
      Object valVal = m_result.getValue( r1, m_valComp );
      int x1, y1, x2, y2;
      if (m_domAxis.getPosition().getOrientation()==ORIENTATION.HORIZONTAL)
      {
        Point xs = m_domAxis.logicalToScreenInterval( domVal,fixedPoint ,barInterval  );
        x1=Math.min(xs.x, xs.y);
        x2=Math.max(xs.x, xs.y);
        
            
        
        y1 = m_valAxis.logicalToScreen( m_valAxis.getFrom() );
        y2= m_valAxis.logicalToScreen( valVal );
      }
      else
      {
        Point ys = m_domAxis.logicalToScreenInterval( domVal,fixedPoint ,barInterval  );
        y1=Math.min(ys.x, ys.y);
        y2=Math.max(ys.x, ys.y);
        x1 = m_valAxis.logicalToScreen( m_valAxis.getFrom() );
        x2=m_valAxis.logicalToScreen( valVal );
      }
      ArrayList<Point> path=new ArrayList<Point>();
      path.add(new Point(x1, y1));
      path.add(new Point(x2, y1));
      path.add(new Point(x2, y2));
      path.add(new Point(x1, y2));
      
      if (sp!=null)
      {
        sp.setPath(path);
        System.out.println("Drawing Bar: ("+x1+":"+y1+"):("+x2+":"+y2+")");
        sp.paint(gc, dev);
      }
    }
   
  }

  /**
   * @see org.kalypso.swtchart.layer.IChartLayer#getName()
   */
  public String getName( )
  {
    return "test";
  }

  /**
   * @see org.kalypso.swtchart.layer.IChartLayer#getDescription()
   */
  public String getDescription( )
  {
    return "";
  }

  /**
   * @see org.kalypso.swtchart.layer.IChartLayer#getDomainAxis()
   */
  public IAxis getDomainAxis( )
  {
    return m_domAxis;
  }

  /**
   * @see org.kalypso.swtchart.layer.IChartLayer#getRangeAxis()
   */
  public IAxis getValueAxis( )
  {
    return m_valAxis;
  }

  /**
   * @see org.kalypso.swtchart.layer.IChartLayer#getLegendItem()
   */
  public ILegendItem getLegendItem( )
  {
    return null;
  }

  /**
   * @see org.kalypso.swtchart.layer.IChartLayer#isActive()
   */
  public boolean isActive( )
  {
    return true;
  }

  /**
   * @see org.kalypso.swtchart.layer.IChartLayer#isVisible()
   */
  public boolean isVisible( )
  {
    return true;
  }

  /**
   * @see org.kalypso.swtchart.layer.IChartLayer#getStyle()
   */
  public ILayerStyle getStyle( )
  {
    return m_style;
  }

  /**
   * @see org.kalypso.swtchart.layer.IChartLayer#setStyle(org.kalypso.swtchart.styles.ILayerStyle)
   */
  public void setStyle( ILayerStyle style )
  {
    m_style=style;
  }

  /**
   * @see org.kalypso.swtchart.layer.IChartLayer#getDomainRange()
   */
  public IDataRange getDomainRange( )
  {
    Object min=null;
    Object max=null;
    AliComparator ac=new AliComparator();
    for (int i=0;i<m_result.size();i++)
    {
      IRecord rec=m_result.get(i);
      Object val=rec.getValue(m_domComp);
      if (min == null || ac.compare(min, val)<0)
        min=val;
      if (max == null || ac.compare(max, val)>0)
        max=val;
    }
    return new DataRange(min, max);
   
   
  }

  /**
   * @see org.kalypso.swtchart.layer.IChartLayer#getValueRange()
   */
  public IDataRange getValueRange( )
  {
    Object min=null;
    Object max=null;
    ComparableComparator cc=new ComparableComparator();
    for (int i=0;i<m_result.size();i++)
    {
      IRecord rec=m_result.get(i);
      Object val=rec.getValue(m_valComp);
      if (min == null || cc.compare(min, val)>0)
        min=val;
      if (max == null || cc.compare(max, val)<0)
        max=val;
    }
   return new DataRange(min, max);
  }
}
