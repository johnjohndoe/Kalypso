/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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
 
 
 history:
  
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
     
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.graphics.sld;

import java.util.ArrayList;

import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

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