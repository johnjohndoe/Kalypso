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
package org.kalypsodeegree_impl.tools;

import java.util.ArrayList;
import java.util.HashMap;

import org.kalypsodeegree.tools.Parameter;
import org.kalypsodeegree.tools.ParameterList;

/**
 * The interface defines the access to a list of paramters that can be used as
 * submitted parameters to methods that may receive an variable list of
 * parameters.
 * <p>
 * ---------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
final public class ParameterList_Impl implements ParameterList
{

  private HashMap params = new HashMap();

  private ArrayList keys = new ArrayList( 100 );

  /**
   * returns the parameter that matches the submitted name. if no parameter can
   * be found <tt>null</tt> will be returned.
   */
  public Parameter getParameter( String name )
  {
    return (Parameter)params.get( name );
  }

  /**
   * adds a new <tt>Parameter</tt> to the list
   */
  public void addParameter( String name, Object value )
  {
    Parameter p = new Parameter_Impl( name, value );
    addParameter( p );
  }

  /**
   * adds a new <tt>Parameter</tt> to the list
   */
  public void addParameter( Parameter param )
  {
    params.put( param.getName(), param );
    keys.add( param.getName() );
  }

  /**
   * returns all <tt>Parameter</tt> s contained within the list as array. it
   * is guarenteered that the arrays isn't <tt>null</tt>
   */
  public Parameter[] getParameters()
  {
    Parameter[] p = new Parameter[keys.size()];
    for( int i = 0; i < keys.size(); i++ )
    {
      p[i] = (Parameter)params.get( keys.get( i ) );
    }
    return p;
  }

  /**
   * returns an array of all <tt>Parameter</tt> s names. it is guarenteered
   * that the arrays isn't <tt>null</tt>
   */
  public String[] getParameterNames()
  {
    String[] s = new String[keys.size()];
    return (String[])keys.toArray( s );
  }

  /**
   * returns an array of all <tt>Parameter</tt> s values. it is guarenteered
   * that the arrays isn't <tt>null</tt>
   */
  public Object[] getParameterValues()
  {
    Object[] o = new Object[keys.size()];
    for( int i = 0; i < o.length; i++ )
    {
      Parameter p = (Parameter)params.get( keys.get( i ) );
      o[i] = p.getValue();
    }
    return o;
  }

  /**
   * removes a parameter from the list
   * 
   * @param name
   *          name of the parameter
   */
  public Parameter removeParameter( String name )
  {
    keys.remove( name );
    return (Parameter)params.remove( name );
  }

  public String toString()
  {
    String ret = null;
    ret = "params = " + params + "\n";
    return ret;
  }

}