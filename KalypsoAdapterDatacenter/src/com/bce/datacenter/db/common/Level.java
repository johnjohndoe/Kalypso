package com.bce.datacenter.db.common;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Vector;

import javax.swing.tree.TreeNode;

import com.bce.datacenter.db.persistent.Persistent;

/**
 * A Level is an abstract element of the tree hierarchy of the datacenter. A
 * Level belongs to exactly one parent, except for the root that has no parent.
 * Thus a given Level may have zero, one or more child Levels.
 * 
 * @author Marc Schlienger
 */
public class Level extends Persistent implements TreeNode
{
  /** parent level */
  private Level m_parent = null;

  /** array of child levels, used as simple caching solution */
  private List m_children = null;

  /** array of containers, used as simple caching solution */
  private List m_objects = null;

  /** some arbitrary description */
  private String m_description;

  /** name as shown in the tree */
  private String m_name;

  /** ref of parent level */
  private int m_parentRef;

  /**
   * constructs a Level based on an identifier with the database
   * @param con
   * @param id
   *          level identifier as existing in the database
   */
  public Level( final Connection con, int id )
  {
    super( con, id, true );
  }

  /**
   * Constructor with parameters
   * @param con
   * @param id
   * @param name
   * @param desc
   * @param parentRef
   */
  public Level( final Connection con, int id, String name, String desc, int parentRef )
  {
    super( con, id, false );

    m_name = name;
    m_description = desc;
    m_parentRef = parentRef;
  }

  /**
   * Returns the root level of the hierarchy
   * @param con
   * 
   * @return the very root level
   */
  public static Level getRoot( final Connection con )
  {
    try
    {
      Statement st = con.createStatement();

      // the root has no parent
      ResultSet rs = st
          .executeQuery( "SELECT LVLID FROM DC_TREELEVEL WHERE PARENTLEVEL = 0" );

      rs.next();

      Level level = null;

      if( rs.getObject( 1 ) != null )
        level = new Level( con, rs.getInt( 1 ) );

      con.commit();

      return level;
    }
    catch( SQLException e )
    {
      e.printStackTrace( System.out );

      try
      {
        con.rollback();
      }
      catch( SQLException e1 )
      {
        e1.printStackTrace();
      }

      return null;
    }
  }

  /**
   * @see javax.swing.tree.TreeNode#getAllowsChildren()
   */
  public boolean getAllowsChildren( )
  {
    return false;
  }

  /**
   * @see javax.swing.tree.TreeNode#getChildAt(int)
   */
  public TreeNode getChildAt( int childIndex )
  {
    return (Level) getChildLevels().get( childIndex );
  }

  /**
   * @see javax.swing.tree.TreeNode#getChildCount()
   */
  public int getChildCount( )
  {
    return getChildLevels().size();
  }

  public List getChildLevels( )
  {
    if( m_children != null )
      return m_children;

    try
    {
      m_children = new Vector();

      PreparedStatement stmt = m_con
          .prepareStatement(
              "SELECT * FROM DC_TREELEVEL WHERE PARENTLEVEL = ? ORDER BY LEVELNAME ASC" );
      stmt.setInt( 1, m_ID );

      ResultSet rs = stmt.executeQuery();

      while( rs.next() )
      {
        Level l = new Level( m_con, rs.getInt( 1 ), rs.getString( 2 ), rs
            .getString( 3 ), rs.getInt( 4 ) );

        m_children.add( l );
      }

      rs.close();
      m_con.commit();
    }
    catch( SQLException e )
    {
      e.printStackTrace();

      try
      {
        m_con.rollback();
      }
      catch( SQLException e1 )
      {
        e1.printStackTrace();
      }
    }

    return m_children;
  }

  public String getDescription( )
  {
    return m_description;
  }

  /**
   * @see javax.swing.tree.TreeNode#getIndex(javax.swing.tree.TreeNode)
   */
  public int getIndex( TreeNode node )
  {
    return getChildLevels().indexOf( node );
  }

  /**
   * @see javax.swing.tree.TreeNode#isLeaf()
   */
  public boolean isLeaf( )
  {
    return getChildLevels().size() == 0;
  }

  public String getName( )
  {
    return m_name;
  }

  public List getObjects( )
  {
    if( m_objects != null )
      return m_objects;

    m_objects = DataObjectFactory.loadChannels( m_con, m_ID );

    return m_objects;
  }

  /**
   * @see javax.swing.tree.TreeNode#getParent()
   */
  public TreeNode getParent( )
  {
    return getParentLevel();
  }

  public Level getParentLevel( )
  {
    if( m_parentRef == 0 )
      return null;
    else if( m_parent == null )
      m_parent = new Level( m_con, m_parentRef );

    return m_parent;
  }

  /**
   * Returns the path (that is concatenation of path of parent level + this
   * name)
   * 
   * @return path of this level,
   */
  public String getPathName( )
  {
    Level parent = getParentLevel();

    if( parent != null )
      return parent.getPathName() + '/' + m_name;
    else

      return '/' + m_name;
  }

  /**
   * @see javax.swing.tree.TreeNode#children()
   */
  public Enumeration children( )
  {
    return Collections.enumeration( getChildLevels() );
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return m_name;
  }

  /**
   * read from db and init members
   */
  protected void dbRead( )
  {
    try
    {
      PreparedStatement stmt = m_con
          .prepareStatement(
              "SELECT LEVELNAME, PARENTLEVEL, LEVELDESCRIPTION FROM DC_TREELEVEL WHERE LVLID = ?" );

      stmt.setInt( 1, m_ID );

      ResultSet set = stmt.executeQuery();

      set.next();

      m_name = set.getString( 1 );
      m_parentRef = set.getInt( 2 );
      m_description = set.getString( 3 );

      set.close();
      stmt.close();

      m_con.commit();

      System.out.println( "parent ref on dbread: " + m_parentRef );

    }
    catch( SQLException e )
    {
      e.printStackTrace();

      try
      {
        m_con.rollback();
      }
      catch( SQLException e1 )
      {
        e1.printStackTrace();
      }
    }
  }

  public int getParentRef( )
  {
    return m_parentRef;
  }
}