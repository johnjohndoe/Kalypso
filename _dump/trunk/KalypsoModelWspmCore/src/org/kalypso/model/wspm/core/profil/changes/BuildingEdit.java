package org.kalypso.model.wspm.core.profil.changes;

import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;

public class BuildingEdit implements IProfilChange
{
  private final IProfilBuilding m_building;

  private final BUILDING_PROPERTY m_property;

  private final Object m_newValue;

  public BuildingEdit( final IProfilBuilding building, final BUILDING_PROPERTY property,
      final Object newValue )
  {
    m_building = building;
    m_property = property;
    m_newValue = newValue;
  }

  /**
   * @throws ProfilDataException
   * @see org.kalypso.model.wspm.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    if (hint!=null) hint.setBuildingDataChanged();
    
    final Object oldValue = m_building.getValueFor( m_property );
    m_building.setValue( m_property, m_newValue );
    return new BuildingEdit( m_building, m_property, oldValue );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
       return m_building;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getPointProperty()
   */
  public POINT_PROPERTY getPointProperty( )
  {
       return null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getValue()
   */
  public Double getValue( )
  {
        return null;
  }

 
}
