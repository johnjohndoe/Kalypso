/*
 * Created on 31.03.2005
  */
package com.bce.eind.core.profil.impl.buildings;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;


import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilPointProperty;
import com.bce.eind.core.profil.ProfilBuildingException;
import com.bce.eind.core.profil.IProfil.BUILDING_TYP;
import com.bce.eind.core.profil.IProfil.BUILDING_VALUES;

/**
 * @author kimwerner
 */
public abstract class AbstractProfilBuilding implements IProfilBuilding
{
  //TODO: m_buildingValues für jede abgeleitete Klasse einfügen
  private final BUILDING_TYP m_buildingTyp;

  protected final List<IProfilPointProperty> m_pointProperties;
  
  private final List<BUILDING_VALUES> m_buildingValues;

  private final HashMap<BUILDING_VALUES, Double> m_values;

  public AbstractProfilBuilding( BUILDING_TYP buildingTyp,List<BUILDING_VALUES> buildingValues)
  {
    m_buildingTyp = buildingTyp;
    m_pointProperties = new LinkedList<IProfilPointProperty>();
    m_buildingValues = Collections.unmodifiableList(buildingValues);
    m_values = new HashMap<BUILDING_VALUES, Double>(buildingValues.size());
    setValues();
  }
  private void setValues()
  {
    for(Iterator<BUILDING_VALUES> vIt = m_buildingValues.iterator();vIt.hasNext();)
    {
      m_values.put(vIt.next(),new Double(0.0));
    }
  }
  /**
   * @return Returns the buildingTyp.
   */
  public BUILDING_TYP getBuildingTyp( )
  {
    return m_buildingTyp;
  }

 
  /* (non-Javadoc)
   * @see com.bce.eind.core.profilinterface.IProfilBuilding#getTableDataKeys()
   */
  public List<IProfilPointProperty> getProfilPointProperties( )
  {
    return Collections.unmodifiableList(m_pointProperties);
  }
  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfilBuilding#getValue(com.bce.eind.core.profilinterface.IProfil.BUILDING_VALUES)
   */
  public double getValue( BUILDING_VALUES buildingValue ) throws ProfilBuildingException
  {
    if( m_values.containsKey( buildingValue ) )
      return m_values.get( buildingValue );
    throw new ProfilBuildingException( "Eigenschaft existiert nicht" );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfilBuilding#setValue(com.bce.eind.core.profilinterface.IProfil.BUILDING_VALUES,
   *      double)
   */
  public void setValue( BUILDING_VALUES buildingValue, double value ) throws ProfilBuildingException
  {
    if( m_values.containsKey( buildingValue ) )
      m_values.put( buildingValue, value );
    throw new ProfilBuildingException( "ungültige Eigenschaft für dieses Gebäude" );
  }
  /* (non-Javadoc)
   * @see com.bce.eind.core.profilinterface.IProfilBuilding#getTableDataKeyCount()
   */
  public int getProfilPointPropertiesCount( )
  {
     return m_pointProperties.size();
  }
  /**
   * @return Returns the buildingValues.
   */
  public List<BUILDING_VALUES> getBuildingValues( )
  {
    return m_buildingValues;
  }
}
