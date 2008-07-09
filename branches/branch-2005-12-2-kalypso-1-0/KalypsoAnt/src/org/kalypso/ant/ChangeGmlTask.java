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

import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.xml.bind.DatatypeConverter;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Changes the contents of a gml file.
 * 
 * Loads (ant-)properties from a gml.
 * 
 * <pre>
 *  <kalypso.changeGml gmlURL="${calc.url}/.calculation">
 *     <property featurepath="" featureProperty="scenarioId" value="a"/>
 *     <property featurepath="" featureProperty="scenarioName" value="b"/>
 *   </kalypso.changeGml>
 * </pre>
 * 
 * @author Gernot Belger
 */
public class ChangeGmlTask extends Task
{
  private final List m_properties = new LinkedList();

  /** Location of the gml which will be changed. */
  private URL m_gmlURL;

  public final URL getGmlURL()
  {
    return m_gmlURL;
  }

  public final void setGmlURL( URL gmlURL )
  {
    m_gmlURL = gmlURL;
  }

  public Property createProperty()
  {
    final Property p = new Property();
    m_properties.add( p );
    return p;
  }

  /**
   * @see org.apache.tools.ant.Task#execute()
   */
  public void execute() throws BuildException
  {
    // validieren
    final URL gmlURL = getGmlURL();
    if( gmlURL == null )
      throw new BuildException( "Property 'gmlURL' must be set." );

    final IFile gmlFile = ResourceUtilities.findFileFromURL( gmlURL );
    if( gmlFile == null )
      throw new BuildException( "Unable to write to: " + gmlURL.toExternalForm() );

    getProject().log( "Ändere gml: " + gmlURL, Project.MSG_DEBUG );

    try
    {
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL );
      for( final Iterator iter = m_properties.iterator(); iter.hasNext(); )
        changeProperty( workspace, (Property)iter.next() );

      final SetContentHelper contentHelper = new SetContentHelper( "Writing: " + gmlURL.toExternalForm() )
      {
        protected void write( final OutputStreamWriter writer ) throws Throwable
        {
          GmlSerializer.serializeWorkspace( writer, workspace );
        }
      };
      gmlFile.refreshLocal(IResource.DEPTH_ONE, new NullProgressMonitor());
      contentHelper.setFileContents( gmlFile, false, true, new NullProgressMonitor() );
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

  private void changeProperty( final GMLWorkspace workspace, final Property property )
  {
    // validate
    final String value = property.getValue();

    final String featureProperty = property.getFeatureProperty();
    if( featureProperty == null || featureProperty.length() == 0 )
      throw new BuildException( "Property 'featureProperty' is not set." );

    final String featureID = property.getFeatureID();
    final String featurePath = property.getFeaturePath();
    if( ( featureID == null || featureID.length() == 0 ) && featurePath == null )
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
        f = (Feature)featureFromPath;
      else
        throw new BuildException( "No feature found with path: " + featurePath );
    }

    final FeatureType featureType = f.getFeatureType();
    final FeatureTypeProperty ftp = featureType.getProperty( featureProperty );

    if( ftp == null )
      throw new BuildException( "Unknwon property: " + featureProperty );

    try
    {
      final Object valueObject = parseType( ftp.getType(), value );
      f.setProperty( featureProperty, valueObject );
    }
    catch( final Throwable e )
    {
      throw new BuildException( "Unparsable value: " + value + " (" + ftp.getName() + ")", e );
    }
  }

  /**
   * Parses the given value according to its type. Value must be foratted according to xml standards. TODO: replace by
   * type handler stuff (in 3.3)
   */
  private Object parseType( final String type, final String value )
  {
    // TODO: support all necessary data types

    if( "java.lang.String".equals( type ) )
      return value;

    if( "java.lang.Integer".equals( type ) )
      return new Integer( DatatypeConverter.parseInt( value ) );

    if( "java.lang.Double".equals( type ) )
      return new Double( DatatypeConverter.parseDouble( value ) );

    if( "java.util.Date".equals( type ) )
      return DatatypeConverter.parseDateTime( value ).getTime();

    throw new UnsupportedOperationException( "Unsupported type: " + type );
  }

  public final static class Property
  {
    /** The value which will be set into the property. */
    private String m_value;

    /** FeaturePath if Feature is selected by featurePath. */
    private String m_featurePath;

    /**
     * ID of Feature which will be selected. If set, featurePath with be ignored.
     */
    private String m_featureID;

    /** Name of Property in Feature. */
    private String m_featureProperty;

    public final String getFeatureID()
    {
      return m_featureID;
    }

    public final void setFeatureID( String featureID )
    {
      m_featureID = featureID;
    }

    public final String getFeaturePath()
    {
      return m_featurePath;
    }

    public final void setFeaturePath( String featurePath )
    {
      m_featurePath = featurePath;
    }

    public final String getFeatureProperty()
    {
      return m_featureProperty;
    }

    public final void setFeatureProperty( String featureProperty )
    {
      m_featureProperty = featureProperty;
    }

    public final String getValue()
    {
      return m_value;
    }

    public final void setValue( final String value )
    {
      m_value = value;
    }
  }
}
