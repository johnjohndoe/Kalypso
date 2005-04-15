package org.kalypso.ogc.gml.serialize;

import java.io.PrintWriter;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;


public final class CsvWriterVisitor implements FeatureVisitor
{
  private final PrintWriter m_writer;
  private final String[] m_props;
  private final String m_delemiter;

  public CsvWriterVisitor( final PrintWriter writer, final String[] props, final String delemiter )
  {
    m_writer = writer;
    m_props = props;
    m_delemiter = delemiter;
    
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    for( int i = 0; i < m_props.length; i++ )
    {
      final String prop = m_props[i];
      final Object property = f.getProperty( prop );
      
      m_writer.print( property );
      
      if( i != m_props.length - 1 )
        m_writer.print( m_delemiter );
    }
    
    m_writer.println();
    
    return true;
  }
  
}