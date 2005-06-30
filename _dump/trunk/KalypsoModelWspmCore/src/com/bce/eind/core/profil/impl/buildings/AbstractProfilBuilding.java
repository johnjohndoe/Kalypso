/*
 * Created on 31.03.2005
 */
package com.bce.eind.core.profil.impl.buildings;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.ProfilBuildingException;
import com.bce.eind.core.profil.ProfilBuildingProperty;
import com.bce.eind.core.profil.ProfilPointProperty;

/**
 * @author kimwerner
 */
public abstract class AbstractProfilBuilding implements IProfilBuilding
{
  private final IProfil.BUILDING_TYP m_buildingTyp;

  protected final List<ProfilPointProperty> m_pointProperties;

  private final HashMap<ProfilBuildingProperty, Double> m_buildingValues = new HashMap<ProfilBuildingProperty, Double>();

  public AbstractProfilBuilding( IProfil.BUILDING_TYP buildingTyp,
      List<ProfilBuildingProperty> buildingProperties )
  {
    m_buildingTyp = buildingTyp;
    m_pointProperties = new LinkedList<ProfilPointProperty>();
    for( Iterator<ProfilBuildingProperty> vIt = buildingProperties.iterator(); vIt.hasNext(); )
    {
      m_buildingValues.put( vIt.next(), new Double( 0.0 ) );
    }
  }

  /**
   * @return Returns the buildingTyp.
   */
  public IProfil.BUILDING_TYP getBuildingTyp( )
  {
    return m_buildingTyp;
  }

  /**
   * 
   * @see com.bce.eind.core.profilinterface.IProfilBuilding#getTableDataKeys()
   */
  public List<ProfilPointProperty> getProfilPointProperties( )
  {
    return Collections.unmodifiableList( m_pointProperties );
  }

  /**
   * 
   * @see com.bce.eind.core.profilinterface.IProfilBuilding#getValue(com.bce.eind.core.profilinterface.IProfil.ProfilBuildingProperty)
   */
  public double getValue( ProfilBuildingProperty buildingValue ) throws ProfilBuildingException
  {
    if( m_buildingValues.containsKey( buildingValue ) )
      return m_buildingValues.get( buildingValue );
    throw new ProfilBuildingException( "Eigenschaft existiert nicht" );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfilBuilding#setValue(com.bce.eind.core.profilinterface.IProfil.ProfilBuildingProperty,
   *      double)
   */
  public void setValue( ProfilBuildingProperty buildingValue, double value )
      throws ProfilBuildingException
  {
    if( m_buildingValues.containsKey( buildingValue ) )
    {
      m_buildingValues.put( buildingValue, value );
    }

    else
      throw new ProfilBuildingException( "ungültige Eigenschaft für dieses Gebäude" );

  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfilBuilding#getTableDataKeyCount()
   */
  public int getProfilPointPropertiesCount( )
  {
    return m_pointProperties.size();
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profil.IProfilBuilding#hasProperty(com.bce.eind.core.profil.ProfilBuildingProperty)
   */
  public boolean hasProperty( ProfilBuildingProperty profilBuildingProperty )
  {
    return m_buildingValues.containsKey( profilBuildingProperty );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profil.IProfilBuilding#getProfilBuildingProperties()
   */
  public Collection<ProfilBuildingProperty> getProfilBuildingProperties( )
  {
    return Collections.unmodifiableCollection( m_buildingValues.keySet() );
  }
}
