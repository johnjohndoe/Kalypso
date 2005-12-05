/**
 * 
 */
package com.bce.eind.core.profil.changes;

import java.util.ArrayList;
import java.util.List;

import com.bce.eind.core.profil.IProfilDevider;

/**
 * @author belger
 */
public class ProfilChangeHint
{
  private boolean m_buildingChanged = false;

  private boolean m_buildingDataChanged = false;

  private boolean m_pointValuesChanged = false;

  private boolean m_pointPropertiesChanged = false;

  private boolean m_pointsChanged = false;

  private List<IProfilDevider> m_movedDeviders = new ArrayList<IProfilDevider>();

  public void setBuildingChanged( )
  {
    m_buildingChanged = true;
  }

  /** true, if building was added or removed or replaced */
  public boolean isBuildingChanged( )
  {
    return m_buildingChanged;
  }

  public void setBuildingDataChanged( )
  {
    m_buildingDataChanged = true;
  }

  /**
   * true, if data of the building was changed
   */
  public boolean isBuildingDataChanged( )
  {
    return m_buildingDataChanged;
  }

  public void setPointValuesChanged( )
  {
    m_pointValuesChanged = true;
  }

  /**
   * true, if values of one ore more point were changed
   */
  public boolean isPointValuesChanged( )
  {
    return m_pointValuesChanged;
  }

  public void setPointPropertiesChanged( )
  {
    m_pointPropertiesChanged = true;
  }

  /**
   * true, if pointProperty was remove or added
   */
  public boolean isPointPropertiesChanged( )
  {
    return m_pointPropertiesChanged;
  }

  public void setPointsChanged( )
  {
    m_pointsChanged = true;
  }

  /**
   * true if points were added or removed
   */
  public boolean isPointsChanged( )
  {
    return m_pointsChanged;
  }

  /** Gibt alle devider zurük, die verschoeben wurden. */
  public void addMovedDeviders( final IProfilDevider devider )
  {
    m_movedDeviders.add( devider );
  }

  public IProfilDevider[] getMovedDeviders( )
  {
    return m_movedDeviders.toArray( new IProfilDevider[m_movedDeviders.size()] );
  }
}
