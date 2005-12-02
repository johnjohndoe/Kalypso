package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.impl.PlainProfil;

public class DeviderRemove extends AbstractChange
{

  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#fireEvent(com.bce.eind.core.profil.IProfilListener)
   */
  @Override
  public void fireEvent( IProfilListener listener )
  {
    listener.onDeviderAdded(this);
    
  }
  public DeviderRemove( final IProfilDevider devider)
  {
    super( devider,null, null);
  }
  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  @Override
  public void doChange(PlainProfil profil) throws ProfilDataException
  {
    m_oldValue = profil.removeDevider((IProfilDevider)m_object);
  
  }
  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#getUndoChange()
   */
  @Override
  public AbstractChange getUndoChange( ) throws ProfilDataException
  {
    final IProfilDevider d = (IProfilDevider)m_object;
    return new DeviderAdd(d.getPoint(),d.getTyp()   );
  }
}
