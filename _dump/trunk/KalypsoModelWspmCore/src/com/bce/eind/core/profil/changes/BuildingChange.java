package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import com.bce.eind.core.profil.impl.buildings.AbstractBuilding;

public class BuildingChange extends AbstractChange
{

  public BuildingChange( final IProfilBuilding building, final BUILDING_PROPERTY property,
      final Object newValue )
  {
    super( building, property, newValue );
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(com.bce.eind.core.profil.IProfil)
   */
  @Override
  public EventToFire doChange( ) throws ProfilDataException
  {
    ((AbstractBuilding)m_object).setValue( (BUILDING_PROPERTY)m_property, m_newValue );

    if( m_property != null )
      return EventToFire.BUILDING_CHANGED;

    if( m_object == null )
      return EventToFire.BUILDING_ADD;

    return EventToFire.BUILDING_REMOVED;
  }

  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#getUndoChange()
   */
  @Override
  public AbstractChange getUndoChange( ) throws ProfilDataException
  {
    final IProfilBuilding b = (IProfilBuilding)m_object;
    final BUILDING_PROPERTY bp = (BUILDING_PROPERTY)m_property;
    final Object oldValue = b.getValueFor( bp );
    return new BuildingChange( b, bp, oldValue );
  }

}
