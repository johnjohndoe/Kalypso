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
package org.kalypso.model.hydrology.binding.model;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Binding class for {http://www.tuhh.de/kalypsoNA}Catchment.
 *
 * @author Gernot Belger
 */
public class Catchment extends AbstractNaModelElement
{
  private static final QName PROP_ZFT = new QName( NS_NAMODELL, "zft" ); //$NON-NLS-1$

  public static final QName FEATURE_CATCHMENT = new QName( NS_NAMODELL, "Catchment" ); //$NON-NLS-1$

  public static final QName LINK_CHANNEL = new QName( NS_NAMODELL, "entwaesserungsStrangMember" ); //$NON-NLS-1$

  public static final QName LINK_OVERFLOW_NODE = new QName( NS_NAMODELL, "izkn_vers" ); //$NON-NLS-1$

  public static final QName LINK_IZKN_NODE = new QName( NS_NAMODELL, "izkn" ); //$NON-NLS-1$

  private static final QName PROPLIST_BODENKORREKTUR_MEMBER = new QName( NS_NAMODELL, "bodenkorrekturmember" ); //$NON-NLS-1$

  public static final QName PROPLIST_GRUNDWASSERABFLUSS_MEMBER = new QName( NS_NAMODELL, "grundwasserabflussMember" ); //$NON-NLS-1$

  public static final QName PROP_GEOM = new QName( NS_NAMODELL, "Ort" ); //$NON-NLS-1$

  private static final QName PROP_RETOB = new QName( NS_NAMODELL, "retob" ); //$NON-NLS-1$

  private static final QName PROP_FAKTOR_RETOB = new QName( NS_NAMODELL, "faktorRetob" ); //$NON-NLS-1$

  private static final QName PROP_RETINT = new QName( NS_NAMODELL, "retint" ); //$NON-NLS-1$

  private static final QName PROP_FAKTOR_RETOB_RETINT = new QName( NS_NAMODELL, "faktorRetobRetint" ); //$NON-NLS-1$

  private static final QName PROP_FAKTOR_RETINT = new QName( NS_NAMODELL, "faktorRetint" ); //$NON-NLS-1$

  private static final QName PROP_AIGW = new QName( NS_NAMODELL, "aigw" ); //$NON-NLS-1$

  private static final QName PROP_FAKTOR_AIGW = new QName( NS_NAMODELL, "faktorAigw" ); //$NON-NLS-1$

  private static final QName PROP_BIANF = new QName( NS_NAMODELL, "bianf" ); //$NON-NLS-1$

  private static final QName PROP_TINT = new QName( NS_NAMODELL, "tint" ); //$NON-NLS-1$

  private static final QName PROP_RINTMX = new QName( NS_NAMODELL, "rintmx" ); //$NON-NLS-1$

  private static final QName PROP_FAKTN = new QName( NS_NAMODELL, "faktn" ); //$NON-NLS-1$

  private static final QName PROP_FAKTOR_RETBAS = new QName( NS_NAMODELL, "faktorRetbas" ); //$NON-NLS-1$

  private static final QName PROP_FAKTOR_RETVS = new QName( NS_NAMODELL, "faktorRetvs" ); //$NON-NLS-1$

  private static final QName PROP_FAKTOR_RETGW = new QName( NS_NAMODELL, "faktorRetgw" ); //$NON-NLS-1$

  private static final QName PROP_FAKTOR_RETKLU = new QName( NS_NAMODELL, "faktorRetklu" ); //$NON-NLS-1$

  private static final QName PROP_RETBAS = new QName( NS_NAMODELL, "retbas" ); //$NON-NLS-1$

  private static final QName PROP_RETVS = new QName( NS_NAMODELL, "retvs" ); //$NON-NLS-1$

  private static final QName PROP_RETGW = new QName( NS_NAMODELL, "retgw" ); //$NON-NLS-1$

  private static final QName PROP_RETKLU = new QName( NS_NAMODELL, "retklu" ); //$NON-NLS-1$

  private static final QName PROP_HGRU = new QName( NS_NAMODELL, "hgru" ); //$NON-NLS-1$

  private static final QName PROP_HGRO = new QName( NS_NAMODELL, "hgro" ); //$NON-NLS-1$

  private static final QName PROP_RTR = new QName( NS_NAMODELL, "rtr" ); //$NON-NLS-1$

  private static final QName PROP_PORS = new QName( NS_NAMODELL, "pors" ); //$NON-NLS-1$

  private static final QName PROP_GWSENT = new QName( NS_NAMODELL, "gwsent" ); //$NON-NLS-1$

  private static final QName PROP_KLUPOR = new QName( NS_NAMODELL, "klupor" ); //$NON-NLS-1$

  private static final QName PROP_GENERATE_RESULT = new QName( NS_NAMODELL, "generateResult" ); //$NON-NLS-1$

  public static final QName PROPERTY_RESULT_CATEGORY = new QName( NaModelConstants.NS_NAMODELL, "resultCategory" ); //$NON-NLS-1$

  public static final QName PROP_PRECIPITATION_LINK = new QName( NS_NAMODELL, "niederschlagZR" ); //$NON-NLS-1$

  public static final QName PROP_TEMPERATURE_LINK = new QName( NS_NAMODELL, "temperaturZR" ); //$NON-NLS-1$

  public static final QName PROP_EVAPORATION_LINK = new QName( NS_NAMODELL, "verdunstungZR" ); //$NON-NLS-1$

  private static final QName PROP_ZR_SYNTH = new QName( NS_NAMODELL, "synthZR" ); //$NON-NLS-1$

  private static final QName PROP_SNOWTYPE = new QName( NS_NAMODELL, "snowtype" ); //$NON-NLS-1$

  private static final QName PROP_FTEM = new QName( NS_NAMODELL, "ftem" ); //$NON-NLS-1$

  private static final QName PROP_FVER = new QName( NS_NAMODELL, "fver" ); //$NON-NLS-1$

  public static final QName PROPERTY_CORRSEALING = new QName( NS_NAMODELL, "corrSealing" ); //$NON-NLS-1$

  public static final QName PROPERTY_CORR_MAX_PERC = new QName( NS_NAMODELL, "corrMaxPerc" ); //$NON-NLS-1$

  public static final QName PROPERTY_CORR_GW_INFLOW_RATE = new QName( NS_NAMODELL, "corrGWInflowRate" ); //$NON-NLS-1$

  private IFeatureBindingCollection<Bodenschichtkorrektur> m_bodenKorrekturCollection = null;

  private IFeatureBindingCollection<Grundwasserabfluss> m_grundwasserAbflussCollection = null;

  public Catchment( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public Channel getChannel( )
  {
    return (Channel) FeatureHelper.resolveLink( this, LINK_CHANNEL, true );
  }

  public void setChannel( final Channel channel )
  {
    FeatureHelper.setAsLink( this, LINK_CHANNEL, channel );
  }

  public Node getOverflowNode( )
  {
    return (Node) FeatureHelper.resolveLink( this, LINK_OVERFLOW_NODE, true );
  }

  public Node getIzknNode( )
  {
    return (Node) FeatureHelper.resolveLink( this, LINK_IZKN_NODE, true );
  }

  public GM_Polygon getGeometry( )
  {
    return getProperty( PROP_GEOM, GM_Polygon.class );
  }

  public synchronized IFeatureBindingCollection<Bodenschichtkorrektur> getBodenKorrekturCollection( )
  {
    if( m_bodenKorrekturCollection == null )
      m_bodenKorrekturCollection = new FeatureBindingCollection<>( this, Bodenschichtkorrektur.class, PROPLIST_BODENKORREKTUR_MEMBER, true );
    return m_bodenKorrekturCollection;
  }

  public Bodenschichtkorrektur[] getBodenKorrekturFeatures( )
  {
    final IFeatureBindingCollection<Bodenschichtkorrektur> bodenKorrekturCollection = getBodenKorrekturCollection();
    return bodenKorrekturCollection.toArray( new Bodenschichtkorrektur[] {} );
  }

  public synchronized IFeatureBindingCollection<Grundwasserabfluss> getGrundwasserAbflussCollection( )
  {
    if( m_grundwasserAbflussCollection == null )
      m_grundwasserAbflussCollection = new FeatureBindingCollection<>( this, Grundwasserabfluss.class, PROPLIST_GRUNDWASSERABFLUSS_MEMBER, true );
    return m_grundwasserAbflussCollection;
  }

  public Grundwasserabfluss[] getGrundwasserAbflussFeatures( )
  {
    final IFeatureBindingCollection<Grundwasserabfluss> grundwasserAbflussCollection = getGrundwasserAbflussCollection();
    return grundwasserAbflussCollection.toArray( new Grundwasserabfluss[] {} );
  }

  public double getRetob( )
  {
    // TODO: schema defines no default value, check it!
    return getDoubleProperty( PROP_RETOB, 1.0 );
  }

  public double getFaktorRetob( )
  {
    return getDoubleProperty( PROP_FAKTOR_RETOB, 1.0 );
  }

  public double getRetint( )
  {
    // TODO: schema defines no default value, check it!
    return getDoubleProperty( PROP_RETINT, 1.0 );
  }

  public double getFaktorRetint( )
  {
    return getDoubleProperty( PROP_FAKTOR_RETINT, 1.0 );
  }

  public double getAigw( )
  {
    return getDoubleProperty( PROP_AIGW, 1.0 );
  }

  public double getFaktorAigw( )
  {
    // TODO: schema defines no default value, check it!
    return getDoubleProperty( PROP_FAKTOR_AIGW, 1.0 );
  }

  public double getBianf( )
  {
    return getDoubleProperty( PROP_BIANF, 0.0 );
  }

  public double getTint( )
  {
    // TODO: schema defines no default value, check it!
    return getDoubleProperty( PROP_TINT, 0.0 );
  }

  public double getRintmx( )
  {
    // TODO: schema defines no default value, check it!
    return getDoubleProperty( PROP_RINTMX, 0.0 );
  }

  public double getFaktn( )
  {
    return getDoubleProperty( PROP_FAKTN, 1.0 );
  }

  public double getRetbas( )
  {
    return getDoubleProperty( PROP_RETBAS, 1000.0 );
  }

  public double getFaktorRetbas( )
  {
    // TODO: schema defines no default value, check it!
    return getDoubleProperty( PROP_FAKTOR_RETBAS, 1.0 );
  }

  public double getRetgw( )
  {
    return getDoubleProperty( PROP_RETGW, 10000.0 );
  }

  public double getFaktorRetgw( )
  {
    // TODO: schema defines no default value, check it!
    return getDoubleProperty( PROP_FAKTOR_RETGW, 1.0 );
  }

  public double getRetvs( )
  {
    return getDoubleProperty( PROP_RETVS, 2.0 );
  }

  public double getFaktorRetvs( )
  {
    // TODO: schema defines no default value, check it!
    return getDoubleProperty( PROP_FAKTOR_RETVS, 1.0 );
  }

  public double getRetklu( )
  {
    return getDoubleProperty( PROP_RETKLU, 100000.0 );
  }

  public double getFaktorRetklu( )
  {
    // TODO: schema defines no default value, check it!
    return getDoubleProperty( PROP_FAKTOR_RETKLU, 1.0 );
  }

  public double getHgru( )
  {
    // TODO: schema defines no default value, check it!
    return getDoubleProperty( PROP_HGRU, 0.0 );
  }

  public double getHgro( )
  {
    // TODO: schema defines no default value, check it!
    return getDoubleProperty( PROP_HGRO, 0.0 );
  }

  public double getRtr( )
  {
    return getDoubleProperty( PROP_RTR, 1.0 );
  }

  public double getPors( )
  {
    return getDoubleProperty( PROP_PORS, 0.16 );
  }

  public double getGwsent( )
  {
    // TODO: schema defines no default value, check it!
    return getDoubleProperty( PROP_GWSENT, 0.0 );
  }

  public double getKlupor( )
  {
    return getDoubleProperty( PROP_KLUPOR, 1.0 );
  }

  public boolean isGenerateResults( )
  {
    return getBooleanProperty( PROP_GENERATE_RESULT, false );
  }

  public void setGenerateResults( final boolean value )
  {
    setProperty( PROP_GENERATE_RESULT, value );
  }

  public ZmlLink getPrecipitationLink( )
  {
    return new ZmlLink( this, PROP_PRECIPITATION_LINK );
  }

  public ZmlLink getTemperatureLink( )
  {
    return new ZmlLink( this, PROP_TEMPERATURE_LINK );
  }

  public ZmlLink getEvaporationLink( )
  {
    return new ZmlLink( this, PROP_EVAPORATION_LINK );
  }

  public String getSynthZR( )
  {
    return getProperty( PROP_ZR_SYNTH, String.class );
  }

  public void setBianf( final Double bianf )
  {
    setProperty( PROP_BIANF, bianf );
  }

  public void setFaktorRetobRetint( final Double faktorRetobTetint )
  {
    setProperty( PROP_FAKTOR_RETOB_RETINT, faktorRetobTetint );
  }

  public void setFaktn( final Double faktn )
  {
    setProperty( PROP_FAKTN, faktn );
  }

  public void setFaktorAigw( final Double faktorAigw )
  {
    setProperty( PROP_FAKTOR_AIGW, faktorAigw );
  }

  public double getFactorRetobRetint( )
  {
    final Double factor = getProperty( PROP_FAKTOR_RETOB_RETINT, Double.class );
    if( factor == null )
      return 1.0;

    return factor;
  }

  public String getSnowtype( )
  {
    return getProperty( PROP_SNOWTYPE, String.class );
  }

  public double getFver( )
  {
    return getDoubleProperty( PROP_FVER, 0.0 );
  }

  public double getFtem( )
  {
    return getDoubleProperty( PROP_FTEM, 1.0 );
  }

  public IObservation getZft( )
  {
    return getProperty( PROP_ZFT, IObservation.class );
  }

  public double getSumGwwi( )
  {
    double sumGwwi = 0.0;
    for( final Grundwasserabfluss gwa : getGrundwasserAbflussFeatures() )
    {
      final Catchment linkedFE = gwa.getNgwzu();
      if( linkedFE != null )
        sumGwwi += gwa.getGwwi();
    }
    return sumGwwi;
  }

  public double getCorrSealing( )
  {
    return getDoubleProperty( PROPERTY_CORRSEALING, 1.0 );
  }

  public void setCorrSealing( final Double corrSealing )
  {
    setProperty( PROPERTY_CORRSEALING, corrSealing );
  }

  public double getCorrMaxPercolation( )
  {
    return getDoubleProperty( PROPERTY_CORR_MAX_PERC, 1.0 );
  }

  public void setCorrMaxPercolation( final Double corrMaxPerc )
  {
    setProperty( PROPERTY_CORR_MAX_PERC, corrMaxPerc );
  }

  public double getCorrGwInflowRate( )
  {
    return getDoubleProperty( PROPERTY_CORR_GW_INFLOW_RATE, 1.0 );
  }

  public void setCorrGwInflowRater( final Double corrGwInflowRater )
  {
    setProperty( PROPERTY_CORR_GW_INFLOW_RATE, corrGwInflowRater );
  }

  public String getResultCategory( )
  {
    return getProperty( PROPERTY_RESULT_CATEGORY, String.class );
  }
}
