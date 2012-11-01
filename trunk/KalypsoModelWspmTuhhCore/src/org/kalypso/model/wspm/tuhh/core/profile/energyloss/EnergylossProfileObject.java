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
package org.kalypso.model.wspm.tuhh.core.profile.energyloss;

import java.math.BigDecimal;
import java.util.List;

import org.kalypso.model.wspm.core.profil.IProfileMetadata;
import org.kalypso.model.wspm.core.profil.impl.AbstractProfileObject;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;

/**
 * @author Kim Werner
 * @author Holger Albert
 */
public class EnergylossProfileObject extends AbstractProfileObject implements IEnergylossProfileObject
{
  public static final String ID = "urn:ogc:gml:dict:kalypso:model:wspm:tuhh:core:energylossTypes#ENERGYLOSS"; //$NON-NLS-1$

  private static final String PROPERTY_TYPE = "type"; //$NON-NLS-1$

  private static final String PROPERTY_DESCRIPTION = "description"; //$NON-NLS-1$

  private static final String PROPERTY_VALUE = "value"; //$NON-NLS-1$

  public EnergylossProfileObject( )
  {
    super();
  }

  @Override
  public String getType( )
  {
    return ID;
  }

  @Override
  public String[] getProperties( )
  {
    return new String[] { PROPERTY_TYPE, PROPERTY_DESCRIPTION, PROPERTY_VALUE };
  }

  @Override
  public String getPropertyLabel( final String property )
  {
    if( PROPERTY_TYPE.equals( property ) )
      return Messages.getString("EnergylossProfileObject_0"); // Type //$NON-NLS-1$

    if( PROPERTY_DESCRIPTION.equals( property ) )
      return Messages.getString("EnergylossProfileObject_1"); // Description //$NON-NLS-1$

    if( PROPERTY_VALUE.equals( property ) )
      return Messages.getString("EnergylossProfileObject_2"); // Energyloss //$NON-NLS-1$

    return property;
  }

  @Override
  public Energyloss[] getEnergylosses( )
  {
    final EnergylossData energylossData = toEnergylossData();
    return energylossData.getEnergylosses();
  }

  @Override
  public int getSize( )
  {
    final EnergylossData energylossData = toEnergylossData();
    return energylossData.getSize();
  }

  @Override
  public Energyloss getEnergyloss( final int index )
  {
    final EnergylossData energylossData = toEnergylossData();
    return energylossData.getEnergyloss( index );
  }

  @Override
  public void addEnergyloss( final Energyloss energyloss )
  {
    final EnergylossData energylossData = toEnergylossData();
    energylossData.addEnergyloss( energyloss );
    toMetadata( energylossData );
  }

  @Override
  public void addEnergylosses( final List<Energyloss> energyloss )
  {
    final EnergylossData energylossData = toEnergylossData();
    energylossData.addEnergylosses( energyloss );
    toMetadata( energylossData );
  }

  @Override
  public void setEnergyloss( final int index, final Energyloss energyloss )
  {
    final EnergylossData energylossData = toEnergylossData();
    energylossData.setEnergyloss( index, energyloss );
    toMetadata( energylossData );
  }

  @Override
  public void removeEnergyloss( final int index )
  {
    final EnergylossData energylossData = toEnergylossData();
    energylossData.removeEnergyloss( index );
    toMetadata( energylossData );
  }

  private EnergylossData toEnergylossData( )
  {
    final IProfileMetadata metadata = getMetadata();
    final EnergylossConverter energylossConverter = new EnergylossConverter( metadata );
    return energylossConverter.createEnergylossData();
  }

  private void toMetadata( final EnergylossData energylossData )
  {
    /* Clear existing metadata with special keys. */
    final IProfileMetadata metadata = getMetadata();
    final String[] keys = metadata.getKeys();
    for( final String key : keys )
    {
      if( key.startsWith( EnergylossProfileObject.KEY_TYPE ) )
      {
        removeValue( key );
        continue;
      }

      if( key.startsWith( EnergylossProfileObject.KEY_DESCRIPTION ) )
      {
        removeValue( key );
        continue;
      }

      if( key.startsWith( EnergylossProfileObject.KEY_VALUE ) )
      {
        removeValue( key );
        continue;
      }
    }

    /* Add metadata with special keys. */
    final Energyloss[] energylosses = energylossData.getEnergylosses();
    for( int i = 0; i < energylosses.length; i++ )
    {
      final Energyloss energyloss = energylosses[i];
      final String type = energyloss.getType();
      final String description = energyloss.getDescription();
      final BigDecimal value = energyloss.getValue();

      final String typeKey = String.format( "%s_%d", KEY_TYPE, i ); //$NON-NLS-1$
      final String descriptionKey = String.format( "%s_%d", KEY_DESCRIPTION, i ); //$NON-NLS-1$
      final String valueKey = String.format( "%s_%d", KEY_VALUE, i ); //$NON-NLS-1$

      if( type != null && type.length() > 0 )
        setValue( typeKey, type );

      if( description != null && description.length() > 0 )
        setValue( descriptionKey, description );

      if( value != null )
        setBigDecimalValue( valueKey, value );
    }
  }
}