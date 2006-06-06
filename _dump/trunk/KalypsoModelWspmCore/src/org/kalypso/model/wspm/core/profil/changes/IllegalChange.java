/**
 * 
 */
package org.kalypso.model.wspm.core.profil.changes;

import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;

/**
 * @author kimwerner
 */
public class IllegalChange implements IProfilChange
{

  private final String m_message;

  public IllegalChange(  final String msg )
  {
    m_message = msg;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#doChange()
   */
  @SuppressWarnings("unused")
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    /**
     * do nothing
     */
     return null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
        return m_message;
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
