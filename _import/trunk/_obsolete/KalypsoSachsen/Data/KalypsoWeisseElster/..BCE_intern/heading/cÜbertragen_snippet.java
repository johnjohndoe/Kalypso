In DeleteFeatureCommand#delete():
	
	
	for( final Feature kmFeature : m_featuresToDelete )
    {
      if( "KMChannel".equals( kmFeature.getFeatureType().getQName().getLocalPart() ) )
      {
        final List dinge = (List) kmFeature.getProperty( new QName("http://www.tuhh.de/kalypsoNA","KMParameterMember") );
        if( dinge.size() > 0 )
        {
          final Feature ding0 = (Feature) dinge.get( 0 );
          final QName nameC = new QName("http://www.tuhh.de/kalypsoNA","c");
          final Double c = (Double) ding0.getProperty( nameC );
          
          for( final Object obj : dinge )
          {
            final Feature ding = (Feature) obj;
            ding.setProperty( nameC, c );
          }
        }
      }
    }

    if( true )
      return;
