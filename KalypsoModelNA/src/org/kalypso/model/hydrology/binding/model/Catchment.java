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
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Binding class for {http://www.tuhh.de/kalypsoNA}Catchment.
 * 
 * @author Gernot Belger
 */
public class Catchment extends AbstractNaModelElement
{
  public static final QName FEATURE_CATCHMENT = new QName( NS_NAMODELL, "Catchment" ); //$NON-NLS-1$

  private static final QName LINK_CHANNEL = new QName( NS_NAMODELL, "entwaesserungsStrangMember" ); //$NON-NLS-1$

  private static final QName LINK_OVERFLOW_NODE = new QName( NS_NAMODELL, "izkn_vers" ); //$NON-NLS-1$

  public static final QName LINK_IZKN_NODE = new QName( NS_NAMODELL, "izkn" ); //$NON-NLS-1$

  private static final QName PROPLIST_BODENKORREKTUR_MEMBER = new QName( NS_NAMODELL, "bodenkorrekturmember" ); //$NON-NLS-1$

  private static final QName PROPLIST_GRUNDWASSERABFLUSS_MEMBER = new QName( NS_NAMODELL, "grundwasserabflussMember" ); //$NON-NLS-1$

  private static final QName PROP_GEOM = new QName( NS_NAMODELL, "Ort" ); //$NON-NLS-1$

  private static final QName PROP_RETOB = new QName( NS_NAMODELL, "retob" ); //$NON-NLS-1$

  private static final QName PROP_FAKTOR_RETOB = new QName( NS_NAMODELL, "faktorRetob" ); //$NON-NLS-1$

  private static final QName PROP_RETINT = new QName( NS_NAMODELL, "retint" ); //$NON-NLS-1$

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

  private IFeatureBindingCollection<Feature> m_bodenKorrekturCollection = null;

  private IFeatureBindingCollection<Feature> m_grundwasserAbflussCollection = null;

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

  public GM_Surface< ? > getGeometry( )
  {
    return getProperty( PROP_GEOM, GM_Surface.class );
  }

  public synchronized IFeatureBindingCollection<Feature> getBodenKorrekturCollection( )
  {
    if( m_bodenKorrekturCollection == null )
      m_bodenKorrekturCollection = new FeatureBindingCollection<Feature>( this, Feature.class, PROPLIST_BODENKORREKTUR_MEMBER, true );
    return m_bodenKorrekturCollection;
  }

  public Feature[] getBodenKorrekturFeatures( )
  {
    if( m_bodenKorrekturCollection == null )
      return getBodenKorrekturCollection().toArray( new Feature[] {} );
    // if not null, avoid synchronized method
    return m_bodenKorrekturCollection.toArray( new Feature[] {} );
  }

  public synchronized IFeatureBindingCollection<Feature> getgrundwasserAbflussCollection( )
  {
    if( m_grundwasserAbflussCollection == null )
      m_grundwasserAbflussCollection = new FeatureBindingCollection<Feature>( this, Feature.class, PROPLIST_GRUNDWASSERABFLUSS_MEMBER, true );
    return m_bodenKorrekturCollection;
  }

  public Feature[] getgrundwasserAbflussFeatures( )
  {
    if( m_grundwasserAbflussCollection == null )
      return getgrundwasserAbflussCollection().toArray( new Feature[] {} );
    // if not null, avoid synchronized method
    return m_grundwasserAbflussCollection.toArray( new Feature[] {} );
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

}
