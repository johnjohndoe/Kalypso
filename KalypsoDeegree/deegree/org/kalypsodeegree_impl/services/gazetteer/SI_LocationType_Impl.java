/*----------------    FILE HEADER  ------------------------------------------
 
 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de
 
 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.
 
 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.
 
 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 
 Contact:
 
 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de
 
 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de
 
 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.services.gazetteer;

import java.util.ArrayList;

import org.deegree.model.geometry.GM_Object;
import org.deegree.services.gazetteer.GazetteerException;
import org.deegree.services.gazetteer.SI_LocationType;
import org.deegree.services.wcas.metadatadesc.CitedResponsibleParty;

/**
 * 
 * @version $Revision$
 * @author AxxL
 *  
 */
public class SI_LocationType_Impl implements SI_LocationType
{

  // standard SI_LocationType
  private String name = null;

  private String theme = null;

  private String identifier = null;

  private String definition = null;

  private CitedResponsibleParty owner = null;

  private ArrayList parent = null;

  private ArrayList child = null;

  private GM_Object territoryOfUse = null;

  /**
   * 
   *  
   */
  private SI_LocationType_Impl()
  {
    this.parent = new ArrayList();
    this.child = new ArrayList();
  }

  /**
   * 
   *  
   */
  public SI_LocationType_Impl( String name, String theme, String identifier, String definition,
      CitedResponsibleParty owner, SI_LocationType[] parent, SI_LocationType[] child,
      GM_Object territoryOfUse )
  {

    this();

    setName( name );
    setTheme( theme );
    setIdentifier( identifier );
    setDefinition( definition );
    setOwner( owner );
    setParent( parent );
    setChild( child );
    setTerritoryOfUse( territoryOfUse );

  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.services.gazetteer.capabilities.SI_LocationType#getRS_IdentifierTypeName()
   */
  public String getName()
  {
    return this.name;
  }

  /**
   *  
   */
  protected void setName( String name )
  {
    this.name = name;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.services.gazetteer.capabilities.SI_LocationType#getTheme()
   */
  public String getTheme()
  {
    return this.theme;
  }

  /**
   *  
   */
  protected void setTheme( String theme )
  {
    this.theme = theme;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.services.gazetteer.capabilities.SI_LocationType#getIdentifier()
   */
  public String getIdentifier()
  {
    return this.identifier;
  }

  /**
   * 
   * @param identifier
   */
  protected void setIdentifier( String identifier )
  {
    this.identifier = identifier;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.services.gazetteer.capabilities.SI_LocationType#getDefinition()
   */
  public String getDefinition()
  {
    return this.definition;
  }

  /**
   * 
   * @param definition
   */
  protected void setDefinition( String definition )
  {
    this.definition = definition;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.services.gazetteer.capabilities.SI_LocationType#getOwner()
   */
  public CitedResponsibleParty getOwner()
  {
    return this.owner;
  }

  /**
   * 
   * @param owner
   */
  protected void setOwner( CitedResponsibleParty owner )
  {
    this.owner = owner;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.services.gazetteer.capabilities.SI_LocationType#getParent()
   */
  public SI_LocationType[] getParent()
  {
    return (SI_LocationType[])parent.toArray( new SI_LocationType[parent.size()] );
  }

  /**
   * 
   * @param parent
   */
  protected void addParent( SI_LocationType parent )
  {
    this.parent.add( parent );
  }

  /**
   * 
   * @param parent
   */
  protected void setParent( SI_LocationType[] parent )
  {
    this.parent.clear();

    if( parent != null )
    {
      for( int i = 0; i < parent.length; i++ )
      {
        this.parent.add( parent[i] );
      }
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.services.gazetteer.capabilities.SI_LocationType#getChild()
   */
  public SI_LocationType[] getChild()
  {
    return (SI_LocationType[])child.toArray( new SI_LocationType[child.size()] );
  }

  /**
   * 
   * @param child
   */
  protected void addChild( SI_LocationType child )
  {
    this.child.add( child );
  }

  /**
   * 
   * @param child
   */
  protected void setChild( SI_LocationType[] child )
  {
    this.child.clear();

    if( child != null )
    {
      for( int i = 0; i < child.length; i++ )
      {
        this.child.add( child[i] );
      }
    }
  }

  /**
   * 
   * @return
   */
  public SI_LocationType[] getLocationTypes() throws GazetteerException
  {

    System.out.println( this.child );
    if( this.child.size() > 0 & this.parent.size() == 0 )
    {
      return getChild();
    }
    else if( this.parent.size() > 0 & this.child.size() == 0 )
    {
      return getParent();
    }
    else
    {
      throw new GazetteerException(
          "Failure in SI_LocationType.getLocationTypes(): Neither parent nor child defined!" );
    }

  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.services.gazetteer.capabilities.SI_LocationType#getTerritoryOfUse()
   */
  public GM_Object getTerritoryOfUse()
  {
    return this.territoryOfUse;
  }

  /**
   * 
   * @param territoryOfUse
   */
  protected void setTerritoryOfUse( GM_Object territoryOfUse )
  {
    this.territoryOfUse = territoryOfUse;
  }

  /**
   * Returns a toString representation of this object.
   */
  public String toString()
  {
    String ret = null;

    ret = "Name = " + name + "\n";
    ret += ( "Theme = " + theme + "\n" );
    ret += ( "Identifier = " + identifier + "\n" );
    ret += ( "Definition = " + definition + "\n" );
    ret += ( "Owner = " + owner + "\n" );
    ret += ( "Parent = " + parent + "\n" );
    ret += ( "Child = " + child + "\n" );
    ret += ( "TerritoryOfUse = " + territoryOfUse + "\n" );

    return ret;
  }

}