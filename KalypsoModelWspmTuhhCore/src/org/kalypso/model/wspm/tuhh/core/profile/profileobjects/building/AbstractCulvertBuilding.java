/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building;

import org.kalypso.model.wspm.core.profil.impl.AbstractProfileObject;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;

/**
 * @author Holger Albert
 */
public abstract class AbstractCulvertBuilding extends AbstractProfileObject implements ICulvertBuilding
{
  protected static final String PROPERTY_BEZUGSPUNKT_X = "bezugspunktX"; //$NON-NLS-1$

  protected static final String PROPERTY_BEZUGSPUNKT_Y = "bezugspunktY"; //$NON-NLS-1$

  protected static final String PROPERTY_BREITE = "breite"; //$NON-NLS-1$

  protected static final String PROPERTY_SOHLGEFAELLE = "sohlgefaelle"; //$NON-NLS-1$

  protected static final String PROPERTY_RAUHEIT = "rauheit"; //$NON-NLS-1$

  public static final String KEY_BEZUGSPUNKT_X = "DURCHLASS_BEZUGSPUNKT_X"; //$NON-NLS-1$

  public static final String KEY_BEZUGSPUNKT_Y = "DURCHLASS_BEZUGSPUNKT_Y"; //$NON-NLS-1$

  public static final String KEY_BREITE = "DURCHLASS_BREITE"; //$NON-NLS-1$

  public static final String KEY_SOHLGEFAELLE = "DURCHLASS_SOHLGEFAELLE"; //$NON-NLS-1$

  public static final String KEY_RAUHEIT = "DURCHLASS_RAUHEIT"; //$NON-NLS-1$

  public AbstractCulvertBuilding( )
  {
  }

  public AbstractCulvertBuilding( final ICulvertBuilding culvertBuilding )
  {
    copyValues( culvertBuilding );
  }

  @Override
  public String[] getProperties( )
  {
    return new String[] { PROPERTY_BEZUGSPUNKT_X, PROPERTY_BEZUGSPUNKT_Y, PROPERTY_BREITE, PROPERTY_SOHLGEFAELLE, PROPERTY_RAUHEIT };
  }

  @Override
  public String getPropertyLabel( final String property )
  {
    if( PROPERTY_BEZUGSPUNKT_X.equals( property ) )
      return Messages.getString("AbstractCulvertBuilding_0"); // Anchor Width //$NON-NLS-1$

    if( PROPERTY_BEZUGSPUNKT_Y.equals( property ) )
      return Messages.getString("AbstractCulvertBuilding_1"); // Anchor Height //$NON-NLS-1$

    if( PROPERTY_BREITE.equals( property ) )
      return Messages.getString("AbstractCulvertBuilding_2"); // Largest Width //$NON-NLS-1$

    if( PROPERTY_SOHLGEFAELLE.equals( property ) )
      return Messages.getString("AbstractCulvertBuilding_3"); // Channel Slope //$NON-NLS-1$

    if( PROPERTY_RAUHEIT.equals( property ) )
      return Messages.getString("AbstractCulvertBuilding_4"); // Roughness //$NON-NLS-1$

    return property;
  }

  @Override
  public Double getBezugspunktX( )
  {
    return getDoubleValue( KEY_BEZUGSPUNKT_X, null );
  }

  @Override
  public Double getBezugspunktY( )
  {
    return getDoubleValue( KEY_BEZUGSPUNKT_Y, null );
  }

  @Override
  public Double getBreite( )
  {
    return getDoubleValue( KEY_BREITE, null );
  }

  @Override
  public Double getSohlgefaelle( )
  {
    return getDoubleValue( KEY_SOHLGEFAELLE, null );
  }

  @Override
  public Double getRauheit( )
  {
    return getDoubleValue( KEY_RAUHEIT, null );
  }

  @Override
  public void setBezugspunktX( final Double bezugspunktX )
  {
    setDoubleValue( KEY_BEZUGSPUNKT_X, bezugspunktX );
  }

  @Override
  public void setBezugspunktY( final Double bezugspunktY )
  {
    setDoubleValue( KEY_BEZUGSPUNKT_Y, bezugspunktY );
  }

  @Override
  public void setBreite( final Double breite )
  {
    setDoubleValue( KEY_BREITE, breite );
  }

  @Override
  public void setSohlgefaelle( final Double sohlgefaelle )
  {
    setDoubleValue( KEY_SOHLGEFAELLE, sohlgefaelle );
  }

  @Override
  public void setRauheit( final Double rauheit )
  {
    setDoubleValue( KEY_RAUHEIT, rauheit );
  }

  @Override
  public void copyValues( final ICulvertBuilding culvertBuilding )
  {
    setBezugspunktX( culvertBuilding.getBezugspunktX() );
    setBezugspunktY( culvertBuilding.getBezugspunktY() );
    setBreite( culvertBuilding.getBreite() );
    setSohlgefaelle( culvertBuilding.getSohlgefaelle() );
    setRauheit( culvertBuilding.getRauheit() );
  }
}