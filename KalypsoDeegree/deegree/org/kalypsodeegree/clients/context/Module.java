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
package org.deegree.clients.context;

import org.deegree.tools.Parameter;
import org.deegree.tools.ParameterList;

/**
 * describes the common access to modules embedded into the GUI described by
 * general extension section of a web map context document within the deegree
 * framework. <p/>a module encapsulates GUI elements and/or functions that are
 * used by the GUI. The concrete implementation of the GUI (e.g. as JSP pages)
 * is responsible for enabling the commuication between the different modules of
 * a context.
 * 
 * @version $Revision$
 * @author $author$
 */
public interface Module
{
  /**
   * returns the name of a module
   * 
   * @return
   */
  String getName();

  /**
   * sets the name of a module
   * 
   * @param name
   */
  void setName( String name );

  /**
   * returns the name of the page/class/file etc. containing the content of the
   * module
   */
  String getContent();

  /**
   * sets the name of the page/class/file etc. containing the content of the
   * module
   * 
   * @param content
   */
  void setContent( String content );

  /**
   * returns true if the module is hidden. this will always be the case for
   * modules that just offers functions to the context. visible modules may
   * offere the capability to be turned to visible or not.
   * 
   * @return
   */
  boolean isHidden();

  /**
   * sets the module to be hidden or visible. modules that only adds functions
   * to a context will ignore this because they are always hidden
   * 
   * @param hidden
   */
  void setHidden( boolean hidden );

  /**
   * returns a list of parameters that will be passed to a class/object etc.
   * that represents a module
   * 
   * @return parameters
   */
  ParameterList getParameter();

  /**
   * sets a list of parameters that will be passed to a class/object etc. that
   * represents a module
   * 
   * @param parameterList
   */
  void setParameter( ParameterList parameterList );

  /**
   * adds a parameter to the list of parameters that will be passed to a
   * class/object etc. that represents a module
   * 
   * @param parameter
   */
  void addParameter( Parameter parameter );

  /**
   * removes a parameter to the list of parameters that will be passed to a
   * class/object etc. that represents a module
   * 
   * @param name
   */
  void removeParameter( String name );

  /**
   * returns the a specific confguration for a module. This may be <tt>null</tt>
   * if the module doesn't need to be configured.
   * 
   * @return
   */
  ModuleConfiguration getModuleConfiguration();

  /**
   * sets the specific configuration for a module.
   * 
   * @param configuration
   */
  void setModuleConfiguration( ModuleConfiguration configuration );

  /**
   * returns a string representing the module type. Possible types are
   * "content", "menu" or "toolbar".
   * 
   * @return type
   */
  String getType();

  /**
   * sets the module type. Possible types are are "content", "menu" or
   * "toolbar". Should the <code>type</code> not match any of those, the
   * default type (content) should be set. type can also be <tt>null</tt>
   * 
   * @param type
   *          the string representing the module type
   */
  void setType( String type );

  /**
   * returns the width of the module in the GUI. If '0' will be returned the GUI
   * can set the with like it is best
   * 
   * @return
   */
  int getWidth();

  /**
   * sets the desired width of the module in the GUI. If '0' ist passed the GUI
   * can set the with like it is best
   * 
   * @param width
   *          desired width of the module
   */
  void setWidth( int width );

  /**
   * returns the height of the module in the GUI. If '0' will be returned the
   * GUI can set the with like it is best
   * 
   * @return
   */
  int getHeight();

  /**
   * sets the desired height of the module in the GUI. If '0' ist passed the GUI
   * can set the with like it is best
   * 
   * @param height
   *          desired width of the module
   */
  void setHeight( int height );

  String[] getModuleJSList();

  void setModuleJSList( String[] list );

  /**
   * return true is the module should has scrollbars in the GUI <br>
   * possible values are
   * <UL>
   * <li>no
   * <li>yes
   * <li>auto
   * </UL>
   * default is auto
   * 
   * @return
   */
  String getScrolling();

  /**
   * @see #getScrolling()
   * @param scroll
   *  
   */
  void setScrolling( String scroll );

}