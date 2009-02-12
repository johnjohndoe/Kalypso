package ejb.event;

import javax.jms.JMSException;
import javax.jms.ObjectMessage;
import javax.jms.Session;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicConnectionFactory;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;
import javax.naming.NamingException;

import ejb.util.Lookup;

public class EJBEventHelper {
    
    public static final String FACTORY    = "ConnectionFactory";
    public static final String TOPIC_NAME = "topic/testTopic";

    private static TopicConnectionFactory topicCF  = null;
    private static Topic                  ejbEvent = null;

    public EJBEventHelper()
        throws NamingException
    {
        topicCF  = (TopicConnectionFactory)Lookup.get(FACTORY);
        ejbEvent = (Topic)Lookup.get(TOPIC_NAME);
    }
    
    public void fireEvent(EJBEvent event)
        throws JMSException
    {
        if(event == null) {
            throw new IllegalArgumentException("event must not be null!");
        }
        TopicConnection tc   = null;
        TopicSession    ts   = null;
        TopicPublisher  tpub = null;
        try {
            tc = topicCF.createTopicConnection();
            ts = tc.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
            tpub = ts.createPublisher(ejbEvent);
            ObjectMessage om = ts.createObjectMessage();
            om.setObject(event);
            tpub.publish(om);
        } finally {
            try  { tpub.close(); } catch(Exception ex) {}
            try  { ts.close(); }    catch(Exception ex) {}
            try  { tc.close(); }    catch(Exception ex) {}
        }
    }

}
