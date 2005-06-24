package org.kalypsodeegree_impl.gml.schema.vistors;

import org.kalypsodeegree_impl.gml.schema.GMLSchema;

public interface GMLSchemaVisitor
{
  boolean visit( GMLSchema schema );
}