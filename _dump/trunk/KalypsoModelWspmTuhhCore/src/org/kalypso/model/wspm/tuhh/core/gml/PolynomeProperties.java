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
package org.kalypso.model.wspm.tuhh.core.gml;

import java.math.BigDecimal;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

/**
 * The special polynome properties from the REIB_CONST calc-type.
 * 
 * @author Gernot Belger
 */
public class PolynomeProperties extends AbstractFeatureBinder
{
  private static final QName QNAME_PROP_ALPHALIMIT = new QName( TuhhCalculation.NS_WSPM_TUHH, "alphaLimit" );

  private static final QName QNAME_PROP_INGORE_OUTLIER = new QName( TuhhCalculation.NS_WSPM_TUHH, "ignoreOutlier" );

  private static final QName QNAME_PROP_TRIPPLE_IT = new QName( TuhhCalculation.NS_WSPM_TUHH, "trippleIt" );

  private static final QName QNAME_PROP_TRIPPLE_MODE = new QName( TuhhCalculation.NS_WSPM_TUHH, "trippleMode" );

  public final static QName QNAME = new QName( TuhhCalculation.NS_WSPM_TUHH, "CalcPolynomes" );

  private final static QName QNAME_PROP_DEEGREE = new QName( TuhhCalculation.NS_WSPM_TUHH, "degree" );

  private static final QName QNAME_PROP_RUNOFF_SLOPE = new QName( TuhhCalculation.NS_WSPM_TUHH, "runoffSlope" );

  private static final QName QNAME_PROP_AREA_SLOPE = new QName( TuhhCalculation.NS_WSPM_TUHH, "areaSlope" );

  private static final QName QNAME_PROP_ALPHA_SLOPE = new QName( TuhhCalculation.NS_WSPM_TUHH, "alphaSlope" );

  private static final QName QNAME_PROP_WEIGHT_SPLINE_POINT = new QName( TuhhCalculation.NS_WSPM_TUHH, "weightSplinePoint" );

  public enum TripleMode
  {
    bordvoll,
    slopeChange;
  }

  public PolynomeProperties( final Feature featureToBind )
  {
    super( featureToBind, QNAME );
  }

  public int getDeegree( )
  {
    final Integer value = getProperty( QNAME_PROP_DEEGREE, Integer.class );
    if( value == null )
      return 4;

    return value;
  }

  public boolean getTripleForAll( )
  {
    final Boolean value = getProperty( QNAME_PROP_TRIPPLE_IT, Boolean.class );
    if( value == null )
      return Boolean.FALSE;

    return value;
  }

  public TripleMode getTripleMode( )
  {
    final String value = getProperty( QNAME_PROP_TRIPPLE_MODE, String.class );
    return TripleMode.valueOf( value );
  }

  public boolean getIgnoreOutlier( )
  {
    final Boolean value = getProperty( QNAME_PROP_INGORE_OUTLIER, Boolean.class );
    if( value == null )
      return false;

    return value;
  }

  public double getAlphaLimit( )
  {
    final BigDecimal value = getProperty( QNAME_PROP_ALPHALIMIT, BigDecimal.class );
    if( value == null )
      return 01.40;

    return value.doubleValue();
  }

  public BigDecimal getRunoffSlope( )
  {
    final BigDecimal value = getProperty( QNAME_PROP_RUNOFF_SLOPE, BigDecimal.class );
    if( value == null )
      return new BigDecimal( "02.0000" );

    return value;
  }

  public BigDecimal getAreaSlope( )
  {
    final BigDecimal value = getProperty( QNAME_PROP_AREA_SLOPE, BigDecimal.class );
    if( value == null )
      return new BigDecimal( "02.0000" );

    return value;
  }

  public BigDecimal getAlphaSlope( )
  {
    final BigDecimal value = getProperty( QNAME_PROP_ALPHA_SLOPE, BigDecimal.class );
    if( value == null )
      return new BigDecimal( "02.0000" );

    return value;
  }

  public BigDecimal getWeightSplinePoint( )
  {
    final BigDecimal value = getProperty( QNAME_PROP_WEIGHT_SPLINE_POINT, BigDecimal.class );
    if( value == null )
      return new BigDecimal( "1.00" );

    return value;
  }

}
