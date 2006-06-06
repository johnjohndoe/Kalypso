/*
 * Created on 31.03.2005
 */
package org.kalypso.model.wspm.core.profil.impl.buildings;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.ProfilDataException;


/**
 * @author kimwerner
 */
public abstract class AbstractBuilding implements IProfilBuilding
{
  protected final BUILDING_TYP m_buildingTyp;

  protected final Map<BUILDING_PROPERTY, Object> m_buildingValues = new LinkedHashMap<BUILDING_PROPERTY, Object>();

  public AbstractBuilding( final BUILDING_TYP buildingTyp,
      final Collection<BUILDING_PROPERTY> properties )
  {
    m_buildingTyp = buildingTyp;

    for( final BUILDING_PROPERTY property : properties )
      m_buildingValues.put( property, new Double( 0.0 ) );
  }

  /**
   * @return Returns the buildingTyp.
   */
  public BUILDING_TYP getTyp( )
  {
    return m_buildingTyp;
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfilBuilding#getValue(org.kalypso.model.wspm.core.profilinterface.IProfil.BUILDING_PROPERTY,
   *      TYPE)
   *      return maybe Null
   */
  public Object getValueFor( BUILDING_PROPERTY buildingValue ) throws ProfilDataException
  {
    if( m_buildingValues.containsKey( buildingValue ) )
      return m_buildingValues.get( buildingValue );
    throw new ProfilDataException( "Die Eigenschaft "+buildingValue.toString()+" wird von diesem Bauwerk nicht unterstützt." );
  }

  /**
   * @return oldvalue maybe Null
   * @see org.kalypso.model.wspm.core.profilinterface.IProfilBuilding#setValue(org.kalypso.model.wspm.core.profilinterface.IProfil.BUILDING_PROPERTY,
   *      Object)
   */
  public Object setValue( final BUILDING_PROPERTY property, final Object value )
      throws ProfilDataException
  {
    if( !m_buildingValues.containsKey( property ) )
      throw new ProfilDataException(  "Die Eigenschaft "+property.toString()+" wird von diesem Bauwerk nicht unterstützt.");

    final Object oldValue = m_buildingValues.get( property );

    m_buildingValues.put( property, value );

    return oldValue;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilBuilding#hasProperty(org.kalypso.model.wspm.core.profil.BUILDING_PROPERTY)
   */
  public boolean hasProperty( BUILDING_PROPERTY profilBuildingProperty )
  {
    return m_buildingValues.containsKey( profilBuildingProperty );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilBuilding#getBuildingProperties()
   */
  public Collection<BUILDING_PROPERTY> getBuildingProperties( )
  {
    return Collections.unmodifiableCollection( m_buildingValues.keySet() );
  }
}
