// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/services/gazetteer/SI_LocationInstance_Impl.java,v
// 1.10 2004/07/09 07:19:53 poth Exp $
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

import java.net.URL;
import java.util.Date;

import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.services.gazetteer.SI_LocationInstance;
import org.deegree.services.gazetteer.SI_LocationType;
import org.deegree.services.wcas.metadatadesc.CitedResponsibleParty;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.feature.Feature_Impl;
import org.deegree_impl.model.geometry.GeometryFactory;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.1
 */
public class SI_LocationInstance_Impl extends Feature_Impl implements SI_LocationInstance
{

  private static FeatureType ft = null;

  private static FeatureProperty[] fp = null;

  // static initialization of the feature type of a LocationInstance because
  // all location instances has the same feature type
  static
  {
    try
    {
      ft = initLIFeatureType();
      fp = initLIFeatureProperties();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * Creates a new SI_LocationInstance_Impl object.
   * 
   * @param geographicIdentifier
   * @param alternativeGeographicIdentifier
   * @param begin
   * @param end
   * @param position
   * @param geographicExtent
   * @param administrator
   * @param parents
   * @param child
   * @param locationType
   */
  SI_LocationInstance_Impl( String geographicIdentifier, String alternativeGeographicIdentifier,
      String identifier, Date begin, Date end, GM_Point[] position, GM_Object[] geographicExtent,
      CitedResponsibleParty administrator, SI_LocationInstance[] parents,
      SI_LocationInstance[] child, SI_LocationType locationType )
  {

    super(ft, geographicIdentifier,  fp );
    setGeographicIdentifier( geographicIdentifier );
    setAlternativeGeographicIdentifier( alternativeGeographicIdentifier );
    if( identifier == null )
      identifier = geographicIdentifier;
    setIdentifier( identifier );
    setBegin( begin );
    setEnd( end );
    setPosition( position );
    setGeographicExtent( geographicExtent );
    setAdministrator( administrator );
    setParents( parents );
    setChildren( child );
    setLocationType( locationType );
  }

  /**
   * @return
   */
  public String getAlternativeGeographicIdentifier()
  {
    return (String)getProperty( "alternativeGeographicIdentifier" );
  }

  /**
   * @param name
   */
  public void setAlternativeGeographicIdentifier( String name )
  {
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "alternativeGeographicIdentifier",
        name );
    setProperty( prop );
  }

  /**
   * returns a unique identifier for a location instance
   */
  public String getIdentifier()
  {
    return (String)getProperty( "identifier" );
  }

  /**
   * sets a unique identifier for a location instance
   * 
   * @param identifier
   */
  public void setIdentifier( String identifier )
  {
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "identifier", identifier );
    setProperty( prop );
  }

  /**
   * @return
   */
  public Date getBegin()
  {
    return (Date)getProperty( "begin" );
  }

  /**
   * @param begin
   */
  public void setBegin( Date begin )
  {
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "begin", begin );
    setProperty( prop );
  }

  /**
   * @return
   */
  public SI_LocationInstance[] getChildren()
  {
    return (SI_LocationInstance[])getProperty( "children" );
  }

  /**
   * 
   * @param child
   */
  public void addChildren( SI_LocationInstance child )
  {
    SI_LocationInstance[] agi = getChildren();
    SI_LocationInstance[] nagi = new SI_LocationInstance[agi.length + 1];
    for( int i = 0; i < agi.length; i++ )
    {
      nagi[i] = agi[i];
    }
    nagi[nagi.length - 1] = child;
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "children", nagi );
    setProperty( prop );
  }

  /**
   * @param children
   */
  public void setChildren( SI_LocationInstance[] children )
  {
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "children", children );
    setProperty( prop );
  }

  /**
   * @return
   */
  public Date getEnd()
  {
    return (Date)getProperty( "end" );
  }

  /**
   * @param end
   */
  public void setEnd( Date end )
  {
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "end", end );
    setProperty( prop );
  }

  /**
   * @return
   */
  public CitedResponsibleParty getAdministrator()
  {
    return (CitedResponsibleParty)getProperty( "administrator" );
  }

  /**
   * @param administrator
   */
  public void setAdministrator( CitedResponsibleParty administrator )
  {
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "administrator", administrator );
    setProperty( prop );
  }

  /**
   * @return
   */
  public GM_Object[] getGeographicExtent()
  {
    return (GM_Object[])getProperty( "geographicExtent" );
  }

  /**
   * 
   * @param geographicExtent
   */
  public void addGeographicExtent( GM_Object geographicExtent )
  {
    GM_Object[] agi = getGeographicExtent();
    GM_Object[] nagi = new GM_Object[agi.length + 1];
    for( int i = 0; i < agi.length; i++ )
    {
      nagi[i] = agi[i];
    }
    nagi[nagi.length - 1] = geographicExtent;
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "geographicExtent", nagi );
    setProperty( prop );
  }

  /**
   * @param geographicExtent
   */
  public void setGeographicExtent( GM_Object[] geographicExtent )
  {
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "geographicExtent",
        geographicExtent );
    setProperty( prop );
  }

  /**
   * @return
   */
  public String getGeographicIdentifier()
  {
    return (String)getProperty( "geographicIdentifier" );
  }

  /**
   * @param geographicIdentifier
   */
  public void setGeographicIdentifier( String geographicIdentifier )
  {
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "geographicIdentifier",
        geographicIdentifier );
    setProperty( prop );
  }

  /**
   * @return
   */
  public SI_LocationType getLocationType()
  {
    return (SI_LocationType)getProperty( "locationType" );
  }

  /**
   * @param type
   */
  public void setLocationType( SI_LocationType type )
  {
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "locationType", type );
    setProperty( prop );
  }

  /**
   * @return
   */
  public SI_LocationInstance[] getParents()
  {
    return (SI_LocationInstance[])getProperty( "parents" );
  }

  /**
   * 
   * @param parent
   */
  public void addParents( SI_LocationInstance parent )
  {
    SI_LocationInstance[] agi = getParents();
    SI_LocationInstance[] nagi = new SI_LocationInstance[agi.length + 1];
    for( int i = 0; i < agi.length; i++ )
    {
      nagi[i] = agi[i];
    }
    nagi[nagi.length - 1] = parent;
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "parents", nagi );
    setProperty( prop );
  }

  /**
   * @param parents
   */
  public void setParents( SI_LocationInstance[] parents )
  {
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "parents", parents );
    setProperty( prop );
  }

  /**
   * @return
   */
  public GM_Point[] getPosition()
  {
    return (GM_Point[])getProperty( "position" );
  }

  /**
   * 
   * @param position
   */
  public void addPosition( GM_Point position )
  {
    GM_Point[] agi = getPosition();
    GM_Point[] nagi = new GM_Point[agi.length + 1];
    for( int i = 0; i < agi.length; i++ )
    {
      nagi[i] = agi[i];
    }
    nagi[nagi.length - 1] = position;
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "position", nagi );
    setProperty( prop );
  }

  /**
   * @param position
   */
  public void setPosition( GM_Point[] position )
  {
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "position", position );
    setProperty( prop );
  }

  /**
   * returns an <tt>URL</tt> containing a request against a WFS returning the
   * feature that is the source of this <tt>SI_LocationInstance</tt>
   * 
   * @return
   */
  public URL getSourceFeature()
  {
    return (URL)getProperty( "sourceFeature" );
  }

  /**
   * @see SI_LocationInstance_Impl#getSourceFeature()
   * 
   * @param sourceFeature
   */
  public void setSourceFeature( URL sourceFeature )
  {
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "sourceFeature", sourceFeature );
    setProperty( prop );
  }

  public int compareTo( Object o )
  {
    SI_LocationInstance li = (SI_LocationInstance)o;
    return li.getGeographicIdentifier().compareTo( this.getGeographicIdentifier() );
  }

  private static FeatureType initLIFeatureType()
  {
    FeatureTypeProperty[] ftp = new FeatureTypeProperty[12];
    ftp[0] = FeatureFactory.createFeatureTypeProperty( "geographicIdentifier", "java.lang.String",
        false );
    ftp[1] = FeatureFactory.createFeatureTypeProperty( "alternativeGeographicIdentifier",
        "java.lang.String[]", true );
    ftp[2] = FeatureFactory.createFeatureTypeProperty( "identifier", "java.lang.String", false );
    ftp[3] = FeatureFactory.createFeatureTypeProperty( "begin", "java.util.Date", true );
    ftp[4] = FeatureFactory.createFeatureTypeProperty( "end", "java.util.Date", true );
    ftp[5] = FeatureFactory.createFeatureTypeProperty( "position",
        "org.deegree.model.geometry.GM_Point", true );
    ftp[6] = FeatureFactory.createFeatureTypeProperty( "geographicExtent",
        "org.deegree.model.geometry.GM_Object", true );
    ftp[7] = FeatureFactory.createFeatureTypeProperty( "administrator",
        "org.deegree.services.wcas.metadatadesc.CitedResponsibleParty", true );
    ftp[8] = FeatureFactory.createFeatureTypeProperty( "parents",
        "org.deegree.services.gazetteer.SI_LocationInstance[]", true );
    ftp[9] = FeatureFactory.createFeatureTypeProperty( "children",
        "org.deegree.services.gazetteer.SI_LocationInstance[]", true );
    ftp[10] = FeatureFactory.createFeatureTypeProperty( "locationType",
        "org.deegree.services.gazetteer.SI_LocationType", true );
    ftp[11] = FeatureFactory.createFeatureTypeProperty( "sourceFeature", "java.net.URL", true );
    return FeatureFactory.createFeatureType( null, null, "SI_LocationInstance", ftp );
  }

  private static FeatureProperty[] initLIFeatureProperties() throws Exception
  {
    FeatureProperty[] fp = new FeatureProperty[12];
    fp[0] = FeatureFactory.createFeatureProperty( "geographicIdentifier", "-" );
    fp[1] = FeatureFactory.createFeatureProperty( "alternativeGeographicIdentifier", null );
    fp[2] = FeatureFactory.createFeatureProperty( "identifier", null );
    fp[3] = FeatureFactory.createFeatureProperty( "begin", null );
    fp[4] = FeatureFactory.createFeatureProperty( "end", null );
    fp[5] = FeatureFactory.createFeatureProperty( "position", GeometryFactory.createGM_Point(
        -9E99, -9E99, null ) );
    GM_Envelope env = GeometryFactory.createGM_Envelope( -9E9, -9E9, 9E9, 9E9 );
    fp[6] = FeatureFactory.createFeatureProperty( "position", GeometryFactory.createGM_Surface(
        env, null ) );
    fp[7] = FeatureFactory.createFeatureProperty( "administrator", null );
    fp[8] = FeatureFactory.createFeatureProperty( "parents", null );
    fp[9] = FeatureFactory.createFeatureProperty( "children", null );
    fp[10] = FeatureFactory.createFeatureProperty( "locationType", null );
    fp[11] = FeatureFactory.createFeatureProperty( "sourceFeature", null );
    return fp;
  }

}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * SI_LocationInstance_Impl.java,v $ Revision 1.10 2004/07/09 07:19:53 poth no
 * message
 * 
 * Revision 1.9 2004/04/07 06:43:49 poth no message
 * 
 * Revision 1.8 2004/03/26 16:42:18 poth no message
 * 
 * Revision 1.7 2004/03/24 08:12:20 poth no message
 * 
 * Revision 1.6 2004/03/15 16:55:29 poth no message
 * 
 * Revision 1.5 2004/03/15 07:34:39 poth no message
 * 
 * 
 *  
 ******************************************************************************/