/*
 * Created on 04.08.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.dialogs;

import java.util.ArrayList;

import org.deegree_impl.services.wfs.filterencoding.OperationDefines;

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
  //public static final int PROPERTYISNOTEQUALTO = 108;

  public FilterDialogTreeNode parent = null;

  public ArrayList children = null;

  private String name = null;

  private AbstractData data = null;

  private int type = -1;

  private int subtype = -1;

  private FilterDialogTreeNode()
  {/**/}

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
    if( type >= OperationDefines.PROPERTYISEQUALTO
        && type <= OperationDefines.PROPERTYISGREATERTHANOREQUALTO )
      return true;
    return false;
  }

  public String getName()
  {
    return name;
  }

  public boolean validate()
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
    children = new ArrayList();
    children.add( parent );
  }

  public void addNode( FilterDialogTreeNode node )
  {
    if( children == null )
    {
      children = new ArrayList();
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

  public Object[] getChildren()
  {
    if( children == null )
    {
      children = new ArrayList();
    }
    return children.toArray();
  }

  public int getType()
  {
    return type;
  }

  public int getSubType()
  {
    return subtype;
  }

  public FilterDialogTreeNode getParent()
  {
    return parent;
  }

  public AbstractData getData()
  {
    return data;
  }

  public void setData( AbstractData m_data )
  {
    this.data = m_data;
  }
}

