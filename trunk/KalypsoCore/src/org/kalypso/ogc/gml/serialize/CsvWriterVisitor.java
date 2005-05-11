package org.kalypso.ogc.gml.serialize;

import java.io.PrintWriter;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;


public final class CsvWriterVisitor implements FeatureVisitor
{
  private final PrintWriter m_writer;
  private final Map m_props;
  private final String m_delemiter;

  public CsvWriterVisitor( final PrintWriter writer, final Map properties, final String delemiter )
  {
    m_writer = writer;
    m_props = properties;
    m_delemiter = delemiter;
    
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    for( final Iterator propIt = m_props.entrySet().iterator(); propIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)propIt.next();
      final String prop = (String)entry.getKey();
      final String def = (String)entry.getValue();
      
      final Object property = f.getProperty( prop );
      m_writer.print( property == null ? def : property );

      if( propIt.hasNext() )
        m_writer.print( m_delemiter );
    }
    
    m_writer.println();
    
    return true;
  }
}