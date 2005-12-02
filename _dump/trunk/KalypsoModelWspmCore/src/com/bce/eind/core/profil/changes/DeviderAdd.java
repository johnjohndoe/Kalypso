package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;
import com.bce.eind.core.profil.impl.PlainProfil;

public class DeviderAdd extends AbstractChange
{

  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#fireEvent(com.bce.eind.core.profil.IProfilListener)
   */
  @Override
  public void fireEvent( IProfilListener listener )
  {
    listener.onDeviderAdded(this);
    
  }
  public DeviderAdd( final IProfilPoint destination, final DEVIDER_TYP deviderTyp )
  {
    super(  null,destination, deviderTyp );
  }
  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  @Override
  public void doChange(PlainProfil profil) throws ProfilDataException
  {
    m_oldValue = profil.addDevider((IProfilPoint)m_property,(DEVIDER_TYP)m_newValue);
  
  }
  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#getUndoChange()
   */
  @Override
  public AbstractChange getUndoChange( ) throws ProfilDataException
  {
    return new DeviderRemove((IProfilDevider) m_oldValue );
  }
}
