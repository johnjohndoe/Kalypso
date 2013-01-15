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
package org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building;

import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.impl.AbstractProfileObject;
import org.kalypso.model.wspm.core.profil.impl.GenericProfileHorizon;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.observation.result.IComponent;

/**
 * @author Kim Werner
 * @author Holger Albert
 */
public class BuildingBruecke extends AbstractProfileObject implements IProfileBuilding
{
  public static final String ID = "urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#BRUECKE"; //$NON-NLS-1$

  /** constant for bridge 'oberkante', does not really belong here, but the ok object is using a generic horizon just for now. */
  public static final String ID_OK = "urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#OK"; //$NON-NLS-1$

  private static final String PROPERTY_BREITE = "breite"; //$NON-NLS-1$

  private static final String PROPERTY_UNTERWASSER = "unterwasser"; //$NON-NLS-1$

  private static final String PROPERTY_FORMBEIWERT = "formbeiwert"; //$NON-NLS-1$

  private static final String PROPERTY_RAUHEIT = "rauheit"; //$NON-NLS-1$

  public static final String KEY_BRUECKE_ID = "BRUECKE_BRUECKE_ID"; //$NON-NLS-1$

  public static final String KEY_BREITE = "BRUECKE_BREITE"; //$NON-NLS-1$

  public static final String KEY_UNTERWASSER = "BRUECKE_UNTERWASSER"; //$NON-NLS-1$

  public static final String KEY_FORMBEIWERT = "BRUECKE_FORMBEIWERT"; //$NON-NLS-1$

  public static final String KEY_RAUHEIT = "BRUECKE_RAUHEIT"; //$NON-NLS-1$

  public BuildingBruecke( final IProfile profile )
  {
    super();

    addPointProperties( profile );
  }

  @Override
  public String getType( )
  {
    return ID;
  }

  @Override
  public String[] getProperties( )
  {
    return new String[] { PROPERTY_BREITE, PROPERTY_UNTERWASSER, PROPERTY_FORMBEIWERT, PROPERTY_RAUHEIT };
  }

  @Override
  public String getPropertyLabel( final String property )
  {
    if( PROPERTY_BREITE.equals( property ) )
      return Messages.getString( "BuildingBruecke_0" ); // Largest Width //$NON-NLS-1$

    if( PROPERTY_UNTERWASSER.equals( property ) )
      return Messages.getString( "BuildingBruecke_1" ); // Downstream Height //$NON-NLS-1$

    if( PROPERTY_FORMBEIWERT.equals( property ) )
      return Messages.getString( "BuildingBruecke_2" ); // Pillar Shape Coefficient //$NON-NLS-1$

    if( PROPERTY_RAUHEIT.equals( property ) )
      return Messages.getString( "BuildingBruecke_3" ); // Roughness //$NON-NLS-1$

    return property;
  }

  public String getBrueckeId( )
  {
    return getValue( KEY_BRUECKE_ID, null );
  }

  public Double getBreite( )
  {
    return getDoubleValue( KEY_BREITE, null );
  }

  public Double getUnterwasser( )
  {
    return getDoubleValue( KEY_UNTERWASSER, null );
  }

  public Double getFormbeiwert( )
  {
    return getDoubleValue( KEY_FORMBEIWERT, null );
  }

  public Double getRauheit( )
  {
    return getDoubleValue( KEY_RAUHEIT, null );
  }

  public void setBrueckeId( final String brueckeId )
  {
    setValue( KEY_BRUECKE_ID, brueckeId );
  }

  public void setBreite( final Double breite )
  {
    setDoubleValue( KEY_BREITE, breite );
  }

  public void setUnterwasser( final Double unterwasser )
  {
    setDoubleValue( KEY_UNTERWASSER, unterwasser );
  }

  public void setFormbeiwert( final Double formbeiwert )
  {
    setDoubleValue( KEY_FORMBEIWERT, formbeiwert );
  }

  public void setRauheit( final Double rauheit )
  {
    setDoubleValue( KEY_RAUHEIT, rauheit );
  }

  public String[] getProfileProperties( )
  {
    return new String[] { IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE };
  }

  private void addPointProperties( final IProfile profile )
  {
    if( profile == null )
      return;

    final IComponent uk = profile.getPointPropertyFor( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );
    if( !profile.hasPointProperty( uk ) )
      profile.addPointProperty( uk, null );

    final IComponent ok = profile.getPointPropertyFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );
    if( !profile.hasPointProperty( ok ) )
      profile.addPointProperty( ok, null );
  }

  public IProfileObject findOkProfileObject( final IProfile profile )
  {
    return findOkProfileObject( this, profile.getProfileObjects() );
  }

  public static IProfileObject findOkProfileObject( final BuildingBruecke bruecke, final IProfileObject[] profileObjects )
  {
    final String brueckeId = bruecke.getBrueckeId();
    if( brueckeId == null || brueckeId.length() == 0 )
      return null;

    for( final IProfileObject profileObject : profileObjects )
    {
      if( !(profileObject instanceof GenericProfileHorizon) )
        continue;

      final String bridgeId = profileObject.getValue( KEY_BRUECKE_ID, null );
      if( bridgeId == null || bridgeId.length() == 0 )
        continue;

      if( brueckeId.equals( bridgeId ) )
        return profileObject;
    }

    return null;
  }
}