/**
 * 
 */
package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

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
   * @see com.bce.eind.core.profil.IProfilChange#doChange()
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
   * @see com.bce.eind.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
        return m_message;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getPointProperty()
   */
  public POINT_PROPERTY getPointProperty( )
  {
       return null;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getValue()
   */
  public Double getValue( )
  {
        return null;
  }

  
}
