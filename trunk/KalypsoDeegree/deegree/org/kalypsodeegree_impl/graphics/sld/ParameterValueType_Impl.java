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
package org.deegree_impl.graphics.sld;

import java.util.ArrayList;

import org.deegree.graphics.sld.ParameterValueType;
import org.deegree.model.feature.Feature;
import org.deegree.services.wfs.filterencoding.Expression;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.deegree.xml.Marshallable;
import org.deegree_impl.tools.Debug;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class ParameterValueType_Impl implements ParameterValueType, Marshallable
{
  private ArrayList components = new ArrayList();

  /**
   * Constructs a new <tt>ParameterValueType_Impl</tt>.
   * <p>
   * 
   * @param components
   *          <tt>String</tt>s/<tt>Expression</tt> s that make up the
   *          contents of the <tt>ParameterValueType_Impl</tt>
   */
  public ParameterValueType_Impl( Object[] components )
  {
    setComponents( components );
  }

  /**
   * Returns the contents (mix of <tt>String</tt>/<tt>Expression</tt>
   * -objects) of this <tt>ParameterValueType</tt>.
   * <p>
   * 
   * @return mix of <tt>String</tt>/<tt>Expression</tt> -objects
   */
  public Object[] getComponents()
  {
    return components.toArray( new Object[components.size()] );
  }

  /**
   * Sets the contents (mix of <tt>String</tt>/<tt>Expression</tt>
   * -objects) of this <tt>ParameterValueType</tt>.
   * <p>
   * 
   * @param components
   *          mix of <tt>String</tt> and <tt>Expression</tt> -objects
   */
  public void setComponents( Object[] components )
  {
    this.components.clear();

    if( components != null )
    {
      for( int i = 0; i < components.length; i++ )
      {
        this.components.add( components[i] );
      }
    }
  }

  /**
   * Concatenates a component (a<tt>String</tt> or an <tt>Expression</tt>
   * -object) to this <tt>ParameterValueType</tt>.
   * <p>
   * 
   * @param component
   *          either a <tt>String</tt> or an <tt>Expression</tt> -object
   */
  public void addComponent( Object component )
  {
    components.add( component );
  }

  /**
   * Removes a component (a<tt>String</tt> or an <tt>Expression</tt>
   * -object) from this <tt>ParameterValueType</tt>.
   * <p>
   * 
   * @param component
   *          either a <tt>String</tt> or an <tt>Expression</tt> -object
   */
  public void removeComponent( Object component )
  {
    components.remove( components.indexOf( component ) );
  }

  /**
   * Returns the actual <tt>String</tt> value of this object. Expressions are
   * evaluated according to the given <tt>Feature</tt> -instance.
   * <p>
   * 
   * @param feature
   *          used for the evaluation of the underlying
   *          'wfs:Expression'-elements
   * @return the (evaluated) String value
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  public String evaluate( Feature feature ) throws FilterEvaluationException
  {
    StringBuffer sb = new StringBuffer();

    for( int i = 0; i < components.size(); i++ )
    {
      Object component = components.get( i );
      if( component instanceof Expression )
      {
        sb.append( ( (Expression)component ).evaluate( feature ) );
      }
      else if( component != null && component instanceof String )
      {
        sb.append( ( (String)component ).trim() );
      }
      else
      {
        sb.append( component );
      }
    }

    return sb.toString();
  }

  /**
   * exports the content of the ParameterValueType as XML formated String
   * 
   * @return xml representation of the ParameterValueType
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer();
    for( int i = 0; i < components.size(); i++ )
    {
      Object component = components.get( i );
      if( component instanceof Expression )
      {
        sb.append( ( (Expression)component ).toXML() );
      }
      else if( component != null && component instanceof String )
      {
        sb.append( ( (String)component ).trim() );
      }
      else
      {
        sb.append( component );
      }
    }

    Debug.debugMethodEnd();
    return sb.toString();
  }

}