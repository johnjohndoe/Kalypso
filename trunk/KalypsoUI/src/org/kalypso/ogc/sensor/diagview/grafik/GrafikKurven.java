/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.diagview.grafik;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.kalypso.java.awt.ColorUtilities;
import org.kalypso.java.util.StringUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.diagview.grafik.GrafikAchsen.GrafikAchse;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.TypeCurve;

/**
 * GrafikKurven
 * 
 * @author schlienger
 */
public class GrafikKurven
{
  private final List m_kurven = new ArrayList();

  private final GrafikAchsen m_achsen;

  /**
   * Constructor with the list of grafik achsen
   * 
   * @param gAchsen
   */
  public GrafikKurven( GrafikAchsen gAchsen )
  {
    m_achsen = gAchsen;
  }

  /**
   * Add a curve, performs the business to make it grafik compliant
   * 
   * @param tc
   * @param numberAxes
   * @return
   * @return
   */
  public IAxis addCurve( final IFile file, final TypeCurve tc, final IAxis[] numberAxes )
  {
    final GrafikKurve gk = new GrafikKurve( file.getName(), tc.getName(), tc
        .isShown(), toGrafikColor( tc.getColor() ) );

    m_kurven.add( gk );

    IAxis axis = null;
    
    final List tmList = tc.getMapping();
    for( Iterator itm = tmList.iterator(); itm.hasNext(); )
    {
      final TypeAxisMapping tm = (TypeAxisMapping) itm.next();

      final GrafikAchse ga = m_achsen.getFor( tm.getDiagramAxis() );
      if( ga != null )
      {
        axis = ObservationUtilities.findAxisByName( numberAxes, tm
            .getObservationAxis() );

        gk.setNr( m_kurven.size() );
        gk.setType( toGrafikType( axis.getType() ) );
        gk.setAxisNr( ga.getId() );
        gk.setUnit( axis.getUnit() );
      }
    }

    return axis;
    //      title = achse.getName() + " (" + title + ")";
  }

  /**
   * @return die Grafik Vorlage welche in der tpl-Datei geschrieben werden soll
   */
  public String toVorlagentext( )
  {
    final StringBuffer sb = new StringBuffer();

    sb
        .append( "/* <Nr>- <Dateiname> <sichtbar:J,N> <Diagr.typ:L,B,P,M,T> <y-Achse:1,2> <Kurventitel> [<Blocknummer>]\n" );
    for( final Iterator it = m_kurven.iterator(); it.hasNext(); )
    {
      final GrafikKurve gk = (GrafikKurve) it.next();

      sb.append( gk.getCurveSpec() ).append( '\n' );
    }

    sb.append( '\n' );

    sb.append( "/* KNr:  Farbe\tLTyp\tLBreite\tPTyp\n" );
    for( final Iterator it = m_kurven.iterator(); it.hasNext(); )
    {
      final GrafikKurve gk = (GrafikKurve) it.next();

      sb.append( gk.getColorSpec() ).append( '\n' );
    }

    return sb.toString();
  }

  /**
   * Returns the same string as the call to toVorlagentext()
   * 
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return toVorlagentext();
  }

  /**
   * Converts the string representation of the color into an integer as used in
   * the grafik template using the getRGB() method of the color class.
   * 
   * @param strColor
   * @return integer representation
   */
  private static int toGrafikColor( final String strColor )
  {
    final Color c;
    
    if( strColor != null )
    {
      // TRICKY: Aus irgendeiner Grund muss man die ROT-BLAU Komponente der Farber
      // wechseln sonst wird die Farbe im Grafik nicht richtig umgesetzt.
      final Color tmp = StringUtilities.stringToColor( strColor );
      c = new Color( tmp.getBlue(), tmp.getGreen(), tmp.getRed() );
    }
    else
      c = ColorUtilities.random();

    // resets the alpha bits since there's no support for it in the grafik tool
    return c.getRGB() & 0x00ffffff;
  }

  /**
   * Convert the string type of the observation axis to a grafik curve type
   * 
   * @param axisType
   * @return grafik curve type
   */
  private static String toGrafikType( final String axisType )
  {
    if( axisType.equals( TimeserieConstants.TYPE_RAINFALL ) )
      return "N";

    return "L";
  }

  /**
   * GrafikKurve
   * 
   * @author schlienger
   */
  private final class GrafikKurve
  {
    private int m_axisNr = 0;

    private String m_type = "X";

    private final String m_name;

    private final int m_color;

    private String m_filename;

    private final String m_shown;

    private int m_nr = 0;

    private String m_unit;

    public GrafikKurve( String filename, String name, boolean shown, int color )
    {
      m_filename = filename;
      m_name = name;
      m_shown = shown ? "J" : "N";
      m_color = color;
    }
    
    /**
     * @param unit
     *          string representation of the unit
     */
    public void setUnit( final String unit )
    {
      m_unit = unit;
    }

    /**
     * @param nr
     *          grafik curve number
     */
    public void setNr( final int nr )
    {
      m_nr = nr;
    }

    /**
     * @param nr
     *          axis number to which this curve is associated
     */
    public void setAxisNr( final int nr )
    {
      m_axisNr = nr;
    }

    /**
     * @param type
     *          grafik curve type (one of L,N,T,... see Grafik-Help)
     */
    public void setType( final String type )
    {
      m_type = type;
    }

    /**
     * @return the grafik color spec for this curve
     */
    public String getColorSpec( )
    {
      final StringBuffer sb = new StringBuffer();
      sb.append( "K" ).append( m_nr ).append( ":\t" ).append( m_color ).append(
          "\t0\t1\t" ).append( m_axisNr );
      return sb.toString();
    }

    /**
     * @return the grafik curve spec for this curve
     */
    public String getCurveSpec( )
    {
      final StringBuffer sb = new StringBuffer();
      sb.append( m_nr ).append( "- " ).append( m_filename ).append( " " )
          .append( m_shown ).append( " " ).append( m_type ).append( " " )
          .append( m_axisNr ).append( " " ).append( m_name ).append( " [" )
          .append( m_unit ).append( "]" );
      return sb.toString();
    }
  }
}