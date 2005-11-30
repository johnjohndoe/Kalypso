package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.ProfilDataException;



public  abstract class AbstractChange
{
  protected final Object m_object;

  protected final Object m_property;

  protected final Object m_newValue;
  
  public static  enum EventToFire {DEVIDER_ADD,DEVIDER_REMOVED,DEVIDER_CHANGED,POINTS_ADD,POINTS_REMOVED,POINTS_CHANGED,PROPERTY_ADD,PROPERTY_REMOVED,BUILDING_ADD,BUILDING_REMOVED,BUILDING_CHANGED,PROFIL_CHANGED};
  
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
  public abstract EventToFire doChange() throws ProfilDataException;
}
