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

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.impl.AbstractProfileObject;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.observation.result.IComponent;

/**
 * @author Kim Werner
 * @author Holger Albert
 */
public class BuildingWehr extends AbstractProfileObject implements IProfileBuilding
{
  public enum WeirType
  {
    // TODO: move strings from WeirLabelProvider into this enum
    beiwert( "Beiwert" ), //$NON-NLS-1$
    breitkronig( "Breitkronig" ), //$NON-NLS-1$
    rundkronig( "Rundkronig" ), //$NON-NLS-1$
    scharfkantig( "Scharfkantig" ); //$NON-NLS-1$

    private final String m_label;

    private WeirType( final String label )
    {
      m_label = label;
    }

    @Override
    public String toString( )
    {
      return m_label;
    }
  }

  public static final String ID = "urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#WEHR"; //$NON-NLS-1$

  private static final String PROPERTY_WEHRART = "wehrart"; //$NON-NLS-1$

  private static final String PROPERTY_FORMBEIWERT = "formbeiwert"; //$NON-NLS-1$

  public static final String KEY_WEHRART = "WEHR_WEHRART"; //$NON-NLS-1$

  public static final String KEY_FORMBEIWERT = "WEHR_FORMBEIWERT"; //$NON-NLS-1$

  public static final WeirType DEFAULT_WEIRTYPE = WeirType.breitkronig;

  public BuildingWehr( final IProfile profile )
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
    return new String[] { PROPERTY_WEHRART, PROPERTY_FORMBEIWERT };
  }

  @Override
  public String getPropertyLabel( final String property )
  {
    if( PROPERTY_WEHRART.equals( property ) )
      return Messages.getString("BuildingWehr_0"); // Weir Type //$NON-NLS-1$

    if( PROPERTY_FORMBEIWERT.equals( property ) )
      return Messages.getString("BuildingWehr_1"); // Pillar Shape Coefficient //$NON-NLS-1$

    return property;
  }

  public WeirType getWehrart( )
  {
    final String value = getValue( KEY_WEHRART, null );
    if( StringUtils.isBlank( value ) )
      return DEFAULT_WEIRTYPE;

    return WeirType.valueOf( value );
  }

  public void setWehrart( final WeirType wehrart )
  {
    setValue( KEY_WEHRART, wehrart.name() );
  }

  public Double getFormbeiwert( )
  {
    return getDoubleValue( KEY_FORMBEIWERT, null );
  }


  public void setFormbeiwert( final Double formbeiwert )
  {
    setDoubleValue( KEY_FORMBEIWERT, formbeiwert );
  }

  public String[] getProfileProperties( )
  {
    return new String[] { IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR };
  }

  private void addPointProperties( final IProfile profile )
  {
    if( profile == null )
      return;

    final IComponent ok = profile.getPointPropertyFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    if( !profile.hasPointProperty( ok ) )
      profile.addPointProperty( ok, null );
  }
}