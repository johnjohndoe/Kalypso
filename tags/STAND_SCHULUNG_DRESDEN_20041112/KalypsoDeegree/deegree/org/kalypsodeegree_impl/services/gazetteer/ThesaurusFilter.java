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

import org.deegree.services.gazetteer.GazetteerException;
import org.deegree.services.gazetteer.SI_LocationType;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.filterencoding.FilterConstructionException;
import org.deegree.xml.ElementList;
import org.deegree.xml.XMLTools;
import org.deegree_impl.services.gazetteer.capabilities.WFSGCapabilitiesFactory;
import org.deegree_impl.services.wfs.filterencoding.AbstractOperation;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.FeatureFilter;
import org.deegree_impl.services.wfs.filterencoding.FeatureId;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 */
public class ThesaurusFilter extends FeatureFilter
{

  private String relationType = null;

  private int hierarchyLevel = 0;

  private SI_LocationType locationType = null;

  private static final String WFSGNS = "http://www.opengis.net/wfs-g";

  /**
   * constructor
   * 
   * @param relationType
   * @param hierarchyLevel
   */
  ThesaurusFilter( String relationType, int hierarchyLevel )
  {
    setRelationType( relationType );
    setHierarchyLevel( hierarchyLevel );
  }

  /**
   * constructor
   * 
   * @param relationType
   * @param hierarchyLevel
   */
  ThesaurusFilter( String relationType, int hierarchyLevel, SI_LocationType locationType )
  {
    setRelationType( relationType );
    setHierarchyLevel( hierarchyLevel );
    setLocationType( locationType );
  }

  /**
   * 
   * @param locationType
   */
  private void setLocationType( SI_LocationType locationType )
  {
    this.locationType = locationType;
  }

  /**
   * 
   * @return
   */
  public SI_LocationType getLocationType()
  {
    return this.locationType;
  }

  /**
   * Given a DOM-fragment, a corresponding Filter-object is built. This method
   * recursively calls other buildFromDOM () - methods to validate the structure
   * of the DOM-fragment.
   * 
   * @throws FilterConstructionException
   *           if the structure of the DOM-fragment is invalid
   */
  public static Filter buildFromDOM( Element element ) throws FilterConstructionException
  {

    Filter filter = null;

    // check if root element's name equals 'filter'
    if( !element.getLocalName().equals( "Filter" ) )
    {
      throw new FilterConstructionException( "Name of element does not equal 'Filter'!" );
    }

    // check if filter is in the correct namespace
    if( !"http://www.opengis.net/ogc".equals( element.getNamespaceURI() ) )
    {
      throw new FilterConstructionException( "Element <" + element.getNodeName()
          + "> is not in the correct namespace: " + element.getNamespaceURI() );
    }

    // determine type of Filter (FeatureFilter / ComplexFilter)
    Element firstElement = null;
    NodeList children = element.getChildNodes();

    for( int i = 0; i < children.getLength(); i++ )
    {
      if( children.item( i ).getNodeType() == Node.ELEMENT_NODE )
      {
        firstElement = (Element)children.item( i );
      }
    }

    if( firstElement == null )
    {
      throw new FilterConstructionException( "Filter Node is empty!" );
    }

    if( firstElement.getLocalName().toLowerCase().equals( "featureid" ) )
    {

      if( WFSGNS.equals( firstElement.getNamespaceURI() ) )
      {
        // it is WFS-G feature filter
        String relationTypeAttribute = firstElement.getAttribute( "relationType" );
        if( ( !relationTypeAttribute.equals( "BT" ) ) && ( !relationTypeAttribute.equals( "NT" ) )
            && ( !relationTypeAttribute.equals( "RT" ) ) )
        {
          throw new FilterConstructionException(
              "Bad ThesaurusFilter: Attribute relationType is \"" + relationTypeAttribute
                  + "\". Must be \"BT\", \"NT\" or \"RT\"." );
        }

        int hierarchyLevelsAttribute = Integer.parseInt( firstElement
            .getAttribute( "hierarchyLevels" ) );
        if( hierarchyLevelsAttribute <= 0 )
        {
          throw new FilterConstructionException(
              "Bad ThesaurusFilter: Attribute hierarchyLevels is \"" + hierarchyLevelsAttribute
                  + "\". Must be greater than zero." );
        }

        ThesaurusFilter tf = new ThesaurusFilter( relationTypeAttribute, hierarchyLevelsAttribute );

        ElementList el = XMLTools.getChildElementsByName( "FeatureId", WFSGNS, element );

        // it is assumed that a filter contained in a WFS-G GetFeature
        // request just contains one FeatureId
        tf.addFeatureId( FeatureId.buildFromDOM( el.item( 0 ) ) );

        element = XMLTools.getNamedChild( el.item( 0 ), WFSGNS, "SI_LocationType" );

        SI_LocationType locationType = null;

        try
        {
          locationType = WFSGCapabilitiesFactory.getSI_LocationType( element );
          tf.setLocationType( locationType );

        }
        catch( GazetteerException e )
        {
          throw new FilterConstructionException( e.getMessage() );
        }

        filter = tf;
      }
      else
      {
        // must be a simple FeatureFilter http://www.opengis.net/ogc
        FeatureFilter fFilter = new FeatureFilter();
        children = element.getChildNodes();

        for( int i = 0; i < children.getLength(); i++ )
        {
          if( children.item( i ).getNodeType() == Node.ELEMENT_NODE )
          {
            Element fid = (Element)children.item( i );

            if( !fid.getLocalName().equals( "FeatureId" ) )
            {
              throw new FilterConstructionException( "Unexpected Element encountered: "
                  + fid.getLocalName() );
            }

            fFilter.addFeatureId( FeatureId.buildFromDOM( fid ) );
          }
        }
        filter = fFilter;
      }
    }
    else
    {
      // must be a ComplexFilter
      children = element.getChildNodes();

      boolean justOne = false;

      for( int i = 0; i < children.getLength(); i++ )
      {
        if( children.item( i ).getNodeType() == Node.ELEMENT_NODE )
        {
          Element operator = (Element)children.item( i );

          if( justOne )
          {
            throw new FilterConstructionException( "Unexpected element encountered: "
                + operator.getLocalName() );
          }

          ComplexFilter cFilter = new ComplexFilter( AbstractOperation.buildFromDOM( operator ) );
          filter = cFilter;
          justOne = true;
        }
      }
    }

    // System.out.println("(Thesaurus)Filter = " + filter);
    return filter;
  }

  /**
   * @return
   */
  public int getHierarchyLevel()
  {
    return this.hierarchyLevel;
  }

  /**
   * @param i
   */
  public void setHierarchyLevel( int i )
  {
    this.hierarchyLevel = i;
  }

  /**
   * @return
   */
  public String getRelationType()
  {
    return this.relationType;
  }

  /**
   * @param string
   */
  public void setRelationType( String string )
  {
    this.relationType = string;
  }

}