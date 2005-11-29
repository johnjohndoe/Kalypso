package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.ProfilDataException;



public  abstract class AbstractChange
{
  protected final Object m_object;

  protected final Object m_property;

  protected final Object m_newValue;

  public AbstractChange( final Object object, final Object property, final Object newValue )
  {
    m_object = object;
    m_property = property;
    m_newValue = newValue;
  }
  
  public Object getObject( )
  {
    return m_object;
  }
  
  public Object getNewValue( )
  {
    return m_newValue;
  }
  
  public Object getProperty( )
  {
    return m_property;
  }
  public abstract AbstractChange getUndoChange() throws ProfilDataException;
  public abstract boolean doChange() throws ProfilDataException;
}
