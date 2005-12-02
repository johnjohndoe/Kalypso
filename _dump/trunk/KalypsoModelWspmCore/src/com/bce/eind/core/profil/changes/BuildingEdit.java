package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import com.bce.eind.core.profil.impl.PlainProfil;
import com.bce.eind.core.profil.impl.buildings.AbstractBuilding;

public class BuildingEdit extends AbstractChange
{

  public BuildingEdit( final IProfilBuilding building, final BUILDING_PROPERTY property,
      final Object newValue )
  {
    super( building, property, newValue );
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  @Override
  public void doChange(final PlainProfil profil ) throws ProfilDataException
  {
    final AbstractBuilding b = (AbstractBuilding)m_object;
    final BUILDING_PROPERTY bp = (BUILDING_PROPERTY)m_property;
    m_oldValue = b.getValueFor( bp );
    b.setValue( bp, m_newValue );
  }

  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#fireEvent(com.bce.eind.core.profil.IProfilListener)
   */
  @Override
  public void fireEvent( IProfilListener listener )
  {
    listener.onBuildingChanged( this );
  }

  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#getUndoChange()
   */
  @Override
  public AbstractChange getUndoChange( ) throws ProfilDataException
  {
    final IProfilBuilding b = (IProfilBuilding)m_object;
    final BUILDING_PROPERTY bp = (BUILDING_PROPERTY)m_property;
    return new BuildingEdit( b, bp, m_oldValue );
  }

}
