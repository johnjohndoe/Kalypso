package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.impl.PlainProfil;

public abstract class AbstractChange
{
  // public static enum EventToFire
  // {
  // BUILDING_ADD, BUILDING_CHANGED, BUILDING_REMOVED, DEVIDER_ADD, DEVIDER_CHANGED,
  // DEVIDER_REMOVED, POINTS_ADD, POINTS_CHANGED, POINTS_REMOVED, PROFIL_CHANGED, PROPERTY_ADD,
  // PROPERTY_REMOVED
  //  }

  protected final Object m_newValue;

  protected final Object m_object;

  protected final Object m_property;
  
  protected  Object m_oldValue;

  public AbstractChange( final Object object, final Object property, final Object newValue )
  {
    m_object = object;
    m_property = property;
    m_newValue = newValue;
  }

  public abstract void doChange(final PlainProfil profil ) throws ProfilDataException;

  public abstract void fireEvent( final IProfilListener listener );

  public Object getNewValue( )
  {
    return m_newValue;
  }

  public Object getObject( )
  {
    return m_object;
  }

  public Object getProperty( )
  {
    return m_property;
  }

  public abstract AbstractChange getUndoChange( ) throws ProfilDataException;
}
