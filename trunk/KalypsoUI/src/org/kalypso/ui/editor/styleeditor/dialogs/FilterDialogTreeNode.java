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
/*
 * Created on 04.08.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.dialogs;

import java.util.ArrayList;

import org.kalypsodeegree_impl.filterencoding.OperationDefines;

/**
 * @author F.Lindemann
 */
public class FilterDialogTreeNode
{

  public static final int ROOT_TYPE = 0;

  public static final int LOGICAL_NODE_TYPE = 1;

  public static final int COMPARISON_NODE_TYPE = 2;

  public static final int FEATUREID_NODE_TYPE = 3;

  // or PARAMETER_TYPE SUCH AS LITERAL, PROPERTY_NAME
  public static final int PARAMETER_TYPE = 4;

  public static final int LOCICAL_NOT = OperationDefines.NOT;

  public static final int LOCICAL_AND = OperationDefines.AND;

  public static final int LOCICAL_OR = OperationDefines.OR;

  public static final int COMPARISON_LIKE = OperationDefines.PROPERTYISLIKE;

  public static final int COMPARISON_NULL = OperationDefines.PROPERTYISNULL;

  public static final int COMPARISON_BETWEEN = OperationDefines.PROPERTYISBETWEEN;

  public static final int COMPARISON_EQUALTO = OperationDefines.PROPERTYISEQUALTO;

  public static final int COMPARISON_LESSTHAN = OperationDefines.PROPERTYISLESSTHAN;

  public static final int COMPARISON_GREATERTHAN = OperationDefines.PROPERTYISGREATERTHAN;

  public static final int COMPARISON_LESSTHANOREQUALTO = OperationDefines.PROPERTYISLESSTHANOREQUALTO;

  public static final int COMPARISON_GREATERTHANOREQUALTO = OperationDefines.PROPERTYISGREATERTHANOREQUALTO;

  // not implemented yet
  // public static final int PROPERTYISNOTEQUALTO = 108;

  public FilterDialogTreeNode parent = null;

  public ArrayList<FilterDialogTreeNode> children = null;

  private String name = null;

  private AbstractData data = null;

  private int type = -1;

  private int subtype = -1;

  private FilterDialogTreeNode( )
  {
    // nothing
  }

  public FilterDialogTreeNode( String string, int m_type )
  {
    this.name = string;
    this.type = m_type;
    switch( type )
    {
      case LOGICAL_NODE_TYPE:
      {
        if( string.equals( "AND" ) )
          subtype = LOCICAL_AND;
        else if( string.equals( "OR" ) )
          subtype = LOCICAL_OR;
        else if( string.equals( "NOT" ) )
          subtype = LOCICAL_NOT;
        break;
      }
      case COMPARISON_NODE_TYPE:
      {
        if( string.equals( "LIKE" ) )
          subtype = COMPARISON_LIKE;
        else if( string.equals( "NULL" ) )
          subtype = COMPARISON_NULL;
        else if( string.equals( "BETWEEN" ) )
          subtype = COMPARISON_BETWEEN;
        else if( string.equals( "EQUAL_TO" ) )
          subtype = COMPARISON_EQUALTO;
        else if( string.equals( "LESS_THAN" ) )
          subtype = COMPARISON_LESSTHAN;
        else if( string.equals( "GREATER_THAN" ) )
          subtype = COMPARISON_GREATERTHAN;
        else if( string.equals( "LESS_THAN_OR_EQUAL_TO" ) )
          subtype = COMPARISON_LESSTHANOREQUALTO;
        else if( string.equals( "GREATER_THAN_OR_EQUAL_TO" ) )
          subtype = COMPARISON_GREATERTHANOREQUALTO;
      }
      default:
        break;
    }

    if( type == ROOT_TYPE )
      createRoot( string );
  }

  public static boolean isBinaryComparisonType( int type )
  {
    if( type >= OperationDefines.PROPERTYISEQUALTO && type <= OperationDefines.PROPERTYISGREATERTHANOREQUALTO )
      return true;
    return false;
  }

  public String getName( )
  {
    return name;
  }

  public boolean validate( )
  {
    if( type == LOGICAL_NODE_TYPE )
    {
      if( subtype == LOCICAL_NOT )
      {
        if( children != null && children.size() == 1 )
          return true;
        return false;
      }
      if( children != null && children.size() > 1 )
        return true;
      return false;
    }
    if( data != null )
      return data.verify();
    return false;

  }

  private void createRoot( String m_name )
  {
    parent = new FilterDialogTreeNode();
    parent.name = m_name;
    parent.type = ROOT_TYPE;
    children = new ArrayList<FilterDialogTreeNode>();
    children.add( parent );
  }

  public void addNode( FilterDialogTreeNode node )
  {
    if( children == null )
    {
      children = new ArrayList<FilterDialogTreeNode>();
    }
    node.parent = this;
    children.add( node );
  }

  public void removeNode( FilterDialogTreeNode node )
  {
    if( children != null )
    {
      children.remove( node );
    }
  }

  public Object[] getChildren( )
  {
    if( children == null )
    {
      children = new ArrayList<FilterDialogTreeNode>();
    }
    return children.toArray();
  }

  public int getType( )
  {
    return type;
  }

  public int getSubType( )
  {
    return subtype;
  }

  public FilterDialogTreeNode getParent( )
  {
    return parent;
  }

  public AbstractData getData( )
  {
    return data;
  }

  public void setData( AbstractData m_data )
  {
    this.data = m_data;
  }
}
