package org.kalypsodeegree_impl.model.sort;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.List;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

public class SplitSortContainer
{
  private static final int MAX_OBJECTS = 500;

  private static final int LEFT_BOTTOM = 0;

  private static final int RIGHT_BOTTOM = 1;

  private static final int RIGHT_TOP = 2;

  private static final int LEFT_TOP = 3;

  private final SplitSortContainer[] m_subContainer = new SplitSortContainer[4];

  private final GM_Envelope m_envelope;

  private final IEnvelopeProvider m_envProvider;

  private List<Object> m_objects = new ArrayList<Object>();

  private SplitSortContainer m_parent;

  public SplitSortContainer( final SplitSortContainer parent, final GM_Envelope env, final IEnvelopeProvider envProvider )
  {
    m_envelope = env;
    m_parent = parent;
    m_envProvider = envProvider;
  }

  public GM_Envelope getEnvelope( )
  {
    return m_envelope;
  }

  public void setParent( final SplitSortContainer container )
  {
    m_parent = container;
  }

  public int size( )
  {
    return m_objects.size();
  }

  private void resort( )
  {
    final List<Object> oldObjects = m_objects;
    m_objects = new ArrayList<Object>( oldObjects.size() );

    for( int i = 0; i < oldObjects.size(); i++ )
    {
      final GM_Envelope env = m_envProvider.getEnvelope( oldObjects.get( i ) );
      if( env != null )
        add( env, oldObjects.get( i ) );
      else
        m_objects.add( oldObjects.get( i ) );
    }
  }

  public void add( final GM_Envelope env, final Object object )
  {
    if( !hasSubContainers() && m_objects.size() < MAX_OBJECTS )
    {
      // as long this container is not too full, add into own list
      m_objects.add( object );
    }
    else if( hasSubContainers() )
    {
      if( m_subContainer[0].getEnvelope().contains( env ) )
        m_subContainer[0].add( env, object );
      else if( m_subContainer[1].getEnvelope().contains( env ) )
        m_subContainer[1].add( env, object );
      else if( m_subContainer[2].getEnvelope().contains( env ) )
        m_subContainer[2].add( env, object );
      else if( m_subContainer[3].getEnvelope().contains( env ) )
        m_subContainer[3].add( env, object );
      else
      {
        // ? Dangerous: all objects may always go into this container...
        m_objects.add( object );
      }
    }
    else
    {
      createSubContainers();
      resort();
      add( env, object );
    }
  }

  public boolean intersects( final GM_Envelope env )
  {
    return m_envelope.intersects( env );
  }

  public void createSubContainers( final SplitSortContainer container )
  {
    double midX = 0d;
    double midY = 0d;
    boolean midXset = false;
    boolean midYset = false;
    final GM_Envelope subEnv = container.getEnvelope();

    final double maxX = m_envelope.getMax().getX();
    final double maxY = m_envelope.getMax().getY();
    final double minX = m_envelope.getMin().getX();
    final double minY = m_envelope.getMin().getY();

    final double maxXsub = subEnv.getMax().getX();
    final double maxYsub = subEnv.getMax().getY();
    final double minXsub = subEnv.getMin().getX();
    final double minYsub = subEnv.getMin().getY();

    if( maxX == maxXsub )
    {
      midX = minXsub;
      midXset = true;
    }
    if( minX == minXsub )
    {
      midX = maxXsub;
      midXset = true;
    }
    if( maxY == maxYsub )
    {
      midY = minYsub;
      midYset = true;
    }
    if( minY == minYsub )
    {
      midY = maxYsub;
      midYset = true;
    }
    if( midXset && midYset )
    {
      createSubContainers( midX, midY );
      for( int i = 0; i < 4; i++ )
      {
        if( m_subContainer[i].getEnvelope().equals( container.getEnvelope() ) )
        {
          m_subContainer[i] = container;
        }
      }
    }
    else
    {
      midX = (minX + maxX) / 2d;
      midY = (minY + maxY) / 2d;
      switch( bestPoint( midX, midY, minXsub, minYsub, maxXsub, maxYsub ) )
      {
        case LEFT_BOTTOM:
          createSubContainers( minXsub, minYsub );
          break;
        case RIGHT_BOTTOM:
          createSubContainers( maxXsub, minYsub );
          break;
        case RIGHT_TOP:
          createSubContainers( maxXsub, maxYsub );
          break;
        case LEFT_TOP:
          createSubContainers( minXsub, maxYsub );
          break;
      }
      for( int i = 0; i < 4; i++ )
      {
        if( m_subContainer[i].getEnvelope().contains( container.getEnvelope() ) )
        {
          m_subContainer[i].createSubContainers( container );
        }
      }
    }
  }

  private int bestPoint( final double midX, final double midY, final double minX, final double minY, final double maxX, final double maxY )
  {
    final double dist[] = new double[4];
    dist[LEFT_BOTTOM] = Math.pow( minX - midX, 2d ) + Math.pow( minY - midY, 2d );
    dist[RIGHT_BOTTOM] = Math.pow( maxX - midX, 2d ) + Math.pow( minY - midY, 2d );
    dist[RIGHT_TOP] = Math.pow( maxX - midX, 2d ) + Math.pow( maxY - midY, 2d );
    dist[LEFT_TOP] = Math.pow( minX - midX, 2d ) + Math.pow( maxY - midY, 2d );
    int result = 0;
    for( int i = 0; i < 4; i++ )
    {
      if( dist[i] < dist[result] )
        result = i;
    }
    return result;
  }

  private void createSubContainers( )
  {
    final double maxX = m_envelope.getMax().getX();
    final double maxY = m_envelope.getMax().getY();

    final double minX = m_envelope.getMin().getX();
    final double minY = m_envelope.getMin().getY();

    final double midX = (minX + maxX) / 2d;
    final double midY = (minY + maxY) / 2d;
    createSubContainers( midX, midY );
  }

  private void createSubContainers( final double midX, final double midY )
  {
    final double maxX = m_envelope.getMax().getX();
    final double maxY = m_envelope.getMax().getY();
    final double minX = m_envelope.getMin().getX();
    final double minY = m_envelope.getMin().getY();

    m_subContainer[0] = new SplitSortContainer( this, GeometryFactory.createGM_Envelope( minX, minY, midX, midY ), m_envProvider );
    m_subContainer[1] = new SplitSortContainer( this, GeometryFactory.createGM_Envelope( midX, minY, maxX, midY ), m_envProvider );
    m_subContainer[2] = new SplitSortContainer( this, GeometryFactory.createGM_Envelope( midX, midY, maxX, maxY ), m_envProvider );
    m_subContainer[3] = new SplitSortContainer( this, GeometryFactory.createGM_Envelope( minX, midY, midX, maxY ), m_envProvider );
  }

  private boolean hasSubContainers( )
  {
    for( int i = 0; i < 4; i++ )
    {
      if( m_subContainer[i] == null )
        return false;
    }

    return true;
  }

  public List query( final GM_Envelope env, List result )
  {
    if( result == null )
      result = new ArrayList();
    if( env == null )
      return result;
    for( int i = 0; i < m_objects.size(); i++ )
    {
      final Object object = m_objects.get( i );
      final GM_Envelope envObject = m_envProvider.getEnvelope( object );
      if( envObject == null || env.intersects( envObject ) )
        result.add( object );
    }
    // boolean queried=false;
    if( hasSubContainers() )
    {
      for( int i = 0; i < 4; i++ )
      {
        if( m_subContainer[i].getEnvelope().contains( env ) )
        {
          result = m_subContainer[i].query( env, result );
          return result;
        }
      }
      for( int i = 0; i < 4; i++ )
      {
        if( env.intersects( m_subContainer[i].getEnvelope() ) )
          m_subContainer[i].query( env, result );
      }
    }
    return result;
  }

  public boolean remove( final Object object )
  {
    final boolean removed = m_objects.remove( object );
    if( removed )
    {
      if( m_parent != null )
        m_parent.optimize();

      return true;
    }

    if( hasSubContainers() )
    {
      for( int i = 0; i < 4; i++ )
      {
        final boolean subRemoved = m_subContainer[i].remove( object );
        if( subRemoved )
          return true;
      }
    }

    return false;
  }

  private void optimize( )
  {
    if( hasSubContainers() )
    {
      for( int i = 0; i < 4; i++ )
      {
        if( m_subContainer[i].hasSubContainers() )
          return;
        if( m_subContainer[i].size() > 0 )
          return;
      }

      for( int i = 0; i < m_subContainer.length; i++ )
        m_subContainer[i] = null;
    }
    if( hasSubContainers() )
      return;
    if( size() > 0 )
      return;
    if( m_parent != null )
      m_parent.optimize();
  }

  public void paint( final Graphics g, final GeoTransform geoTransform )
  {
    final double g1x = geoTransform.getDestX( m_envelope.getMin().getX() );
    final double g1y = geoTransform.getDestY( m_envelope.getMin().getY() );
    final double g2x = geoTransform.getDestX( m_envelope.getMax().getX() );
    final double g2y = geoTransform.getDestY( m_envelope.getMax().getY() );

    g.drawRect( (int) (g1x < g2x ? g1x : g2x), (int) (g1y < g2y ? g1y : g2y), (int) Math.abs( (g2x - g1x) ), (int) Math.abs( (g2y - g1y) ) );

    if( hasSubContainers() )
      for( int i = 0; i < 4; i++ )
        m_subContainer[i].paint( g, geoTransform );
  }
}