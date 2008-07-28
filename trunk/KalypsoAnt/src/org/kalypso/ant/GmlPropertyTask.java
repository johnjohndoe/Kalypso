/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ant;

import java.net.URL;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.lang.time.DateUtils;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.kalypso.contribs.java.util.CalendarUtilities;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Loads (ant-)properties from a gml.
 * 
 * <pre>
 *    &lt;gmlProperty file=&quot;${calc.url}/.calculation&quot;&gt;
 *      &lt;property name=&quot;startsim&quot; featurepath=&quot;&quot;/&gt;
 *      &lt;property name=&quot;stopsim&quot; featurepath=&quot;&quot;/&gt;
 *      &lt;property name=&quot;startforecast&quot; featurepath=&quot;&quot;/&gt;
 *   &lt;/gmlProperty&gt;
 * </pre>
 * 
 * @author belger
 */
public class GmlPropertyTask extends Task
{
  private final PropertyAdder m_propertyAdder = new PropertyAdder( this );

  private final List<Property> m_properties = new LinkedList<Property>();

  /** URL from where to read the gml */
  private URL m_gmlURL;

  public final URL getGmlURL( )
  {
    return m_gmlURL;
  }

  public final void setGmlURL( final URL gmlURL )
  {
    m_gmlURL = gmlURL;
  }

  public Property createProperty( )
  {
    final Property p = new Property();
    m_properties.add( p );
    return p;
  }

  /**
   * @see org.apache.tools.ant.Task#execute()
   */
  @Override
  public void execute( ) throws BuildException
  {
    // validieren
    final URL gmlURL = getGmlURL();
    if( gmlURL == null )
      throw new BuildException( "Property 'gmlURL' must be set." );

    final Project project2 = getProject();
    if( project2 != null )
      project2.log( "Lade properties aus gml: " + gmlURL, Project.MSG_DEBUG );

    try
    {
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL, null );
      for( final Object element : m_properties )
        addProperty( workspace, (Property) element );
    }
    catch( final BuildException be )
    {
      throw be;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new BuildException( "Fehler beim Laden von GML: " + gmlURL, e );
    }
  }

  private void addProperty( final GMLWorkspace workspace, final Property property )
  {
    // validate
    final String name = property.getName();
    if( name == null || name.length() == 0 )
      throw new BuildException( "Property 'name' is not set." );

    final String featureProperty = property.getFeatureProperty();
    if( featureProperty == null || featureProperty.length() == 0 )
      throw new BuildException( "Property 'featureProperty' is not set." );

    final String featureID = property.getFeatureID();
    final String featurePath = property.getFeaturePath();
    if( (featureID == null || featureID.length() == 0) && featurePath == null )
      throw new BuildException( "Neither 'featureID' nor 'featurePath' is set." );

    // find feature
    final Feature f;
    if( featureID != null )
    {
      f = workspace.getFeature( featureID );
      if( f == null )
        throw new BuildException( "No feature for id: " + featureID );
    }
    else
    {
      final Object featureFromPath = workspace.getFeatureFromPath( featurePath );
      if( featureFromPath instanceof Feature )
        f = (Feature) featureFromPath;
      else
        throw new BuildException( "No feature found with path: " + featurePath );
    }

    final Object value = f.getProperty( featureProperty );
    if( value instanceof XMLGregorianCalendar )
    {
      // special handling for Date
      // just write time in millies since 1970
      final Date dateValue = DateUtilities.toDate( (XMLGregorianCalendar) value );
      final Integer dateoffset = property.getDateoffset();
      final String dateoffsetfield = property.getDateoffsetfield();
      final String dateTruncField = property.getDateTruncField();
      Date date;
      if( dateoffset != null && dateoffsetfield != null )
      {
        final Calendar cal = Calendar.getInstance();
        cal.setTime( dateValue );
        cal.add( CalendarUtilities.getCalendarField( dateoffsetfield ), dateoffset.intValue() );
        date = cal.getTime();
      }
      else
        date = dateValue;

      if( dateTruncField != null )
        date = DateUtils.truncate( date, Integer.valueOf( dateTruncField ).intValue() );

      m_propertyAdder.addProperty( name, "" + date.getTime(), null );
    }
    else if( value != null )
      m_propertyAdder.addProperty( name, value.toString(), null );
    else
    {
      final Project project2 = getProject();
      if( project2 != null )
        project2.log( "No value for feature with id " + f.getId() + " in property: " + featureProperty, Project.MSG_DEBUG );
      final String defaultValue = property.getDefaultValue();
      if( defaultValue != null )
      {
        if( project2 != null )
          project2.log( "Using defualt value: " + defaultValue, Project.MSG_DEBUG );
        m_propertyAdder.addProperty( name, defaultValue, null );
      }
    }
  }

  public final static class Property
  {
    /** The name, the property gets */
    private String m_name;

    /** If the value cannot be determined, this value will be used */
    private String m_defaultValue;

    /** FeaturePath if Feature is selected by featurePath. */
    private String m_featurePath;

    /**
     * ID of Feature which will be selected. If set, featurePath with be ignored.
     */
    private String m_featureID;

    /** Name of Property in Feature. */
    private String m_featureProperty;

    /** HACK: if the property is a date, add this offset */
    private Integer m_dateoffset;

    /** HACK: if the property is a date, the offset to this field. Must be One of Calendar.HOUR_OF_DAY, etc. */
    private String m_dateoffsetfield;

    /**
     * HACK: if the property is a date, truncation manipulation to this field. Must be One of Calendar.HOUR_OF_DAY, etc.
     */
    private String m_dateTruncField;

    public final Integer getDateoffset( )
    {
      return m_dateoffset;
    }

    public final void setDateoffset( final Integer dateoffset )
    {
      m_dateoffset = dateoffset;
    }

    public final String getDateoffsetfield( )
    {
      return m_dateoffsetfield;
    }

    public final void setDateoffsetfield( final String dateoffsetfield )
    {
      m_dateoffsetfield = dateoffsetfield;
    }

    public final String getDateTruncField( )
    {
      return m_dateTruncField;
    }

    public final void setDateTruncField( final String dateTruncfield )
    {
      m_dateTruncField = dateTruncfield;
    }

    public final String getFeatureID( )
    {
      return m_featureID;
    }

    public final void setFeatureID( final String featureID )
    {
      m_featureID = featureID;
    }

    public final String getFeaturePath( )
    {
      return m_featurePath;
    }

    public final void setFeaturePath( final String featurePath )
    {
      m_featurePath = featurePath;
    }

    public final String getFeatureProperty( )
    {
      return m_featureProperty;
    }

    public final void setFeatureProperty( final String featureProperty )
    {
      m_featureProperty = featureProperty;
    }

    public final String getName( )
    {
      return m_name;
    }

    public final void setName( final String name )
    {
      m_name = name;
    }

    public final void setDefaultValue( final String defaultValue )
    {
      m_defaultValue = defaultValue;
    }

    public final String getDefaultValue( )
    {
      return m_defaultValue;
    }
  }
}
