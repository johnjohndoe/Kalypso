/**
 * 
 */
package com.bce.eind.core.profil.changes;

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

  private boolean m_deviderDataChanged = false;

  private boolean m_deviderMoved = false;

  private boolean m_ProfilPropertyChanged = false;

  private boolean m_activePointChanged = false;

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

  public void setDeviderMoved( )
  {
    m_deviderMoved = true;
  }

  /**
   * true if one or more devider moved
   */
  public boolean isDeviderMoved( )
  {
    return m_deviderMoved;
  }

  public void setDeviderDataChanged( )
  {
    m_deviderDataChanged = true;
  }

  /**
   * true if one or more devider changed properties
   */
  public boolean isDeviderDataChanged( )
  {
    return m_deviderDataChanged;
  }

  /**
   * true if profilPropertyChanged.
   */
  public boolean isProfilPropertyChanged( )
  {
    return m_ProfilPropertyChanged;
  }

  public void setProfilPropertyChanged( boolean profilPropertyChanged )
  {
    m_ProfilPropertyChanged = profilPropertyChanged;
  }

  public void setActivePointChanged( )
  {
    m_activePointChanged = true;
  }

  /** true, if the active point or the active property of the profil changes. 
   * @return */
  public boolean isActivePointChanged( )
  {
    return m_activePointChanged;
  }
}
