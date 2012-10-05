/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import org.kalypso.model.wspm.core.profil.impl.AbstractProfileObject;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IComponent;

/**
 * @author Kim Werner
 * @author Holger Albert
 */
public class BuildingWehr extends AbstractProfileObject implements IProfileBuilding
{
  public static final String ID = IWspmTuhhConstants.BUILDING_TYP_WEHR;

  private static final String PROPERTY_WEHRART = "wehrart"; //$NON-NLS-1$

  private static final String PROPERTY_FORMBEIWERT = "formbeiwert"; //$NON-NLS-1$

  public static final String KEY_WEHRART = "WEHR_WEHRART"; //$NON-NLS-1$

  public static final String KEY_FORMBEIWERT = "WEHR_FORMBEIWERT"; //$NON-NLS-1$

  public BuildingWehr( final IProfile profile )
  {
    super();

    addPointProperties( profile );
  }

  @Override
  public String getId( )
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
      return "Wehrart"; // Weir Type

    if( PROPERTY_FORMBEIWERT.equals( property ) )
      return "Pfeilerformbeiwert"; // Pillar Shape Coefficient

    return property;
  }

  public String getWehrart( )
  {
    return getValue( KEY_WEHRART, null );
  }

  public Double getFormbeiwert( )
  {
    return getDoubleValue( KEY_FORMBEIWERT, null );
  }

  public void setWehrart( final String wehrart )
  {
    setValue( KEY_WEHRART, wehrart );
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