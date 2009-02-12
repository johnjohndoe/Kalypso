package ejb.event;

import java.util.List;

import javax.ejb.EnterpriseBean;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.ObjectMessage;
import javax.jms.Session;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicConnectionFactory;
import javax.jms.TopicSession;
import javax.jms.TopicSubscriber;
import javax.naming.NamingException;

import ejb.util.Lookup;


public class EJBEventManager implements MessageListener {
    
    //public static final String FACTORY    = "java:/env/jms/DefaultConnectionFactory";
    //public static final String TOPIC_NAME = "java:/env/jms/EJBEvents";
    public static final String FACTORY    = "ConnectionFactory";
    public static final String TOPIC_NAME = "java:topic/testTopic";
    
    private static EJBEventManager        theInstance = null;
    private static TopicConnectionFactory tcFactory   = null;
    private static Topic                  ejbEvents   = null;
    
    private TopicConnection tConnection = null;
    private TopicSession    tSession    = null;
    private TopicSubscriber tSub        = null;
    private List            tListeners  = null;
    
    static {
        try {
            tcFactory = (TopicConnectionFactory)Lookup.get(FACTORY);
            ejbEvents = (Topic)Lookup.get(TOPIC_NAME);
            theInstance = new EJBEventManager();
        } catch(NamingException nex) {
            nex.printStackTrace();
            throw new IllegalStateException(nex.getMessage());
        }
    }

    private EJBEventManager() {
        tListeners = new java.util.ArrayList();
    }
    
    public synchronized void addEJBEventListener(EJBEventListener listener) {
        if(listener instanceof EnterpriseBean) {
            throw new IllegalArgumentException("beans are not allowed!");
        }
        if(tListeners.isEmpty()) {
            connect();
        }
        if(!tListeners.contains(listener)) 
	    {
		System.out.println("add EJBEventListener");
		tListeners.add(listener);
	    }
    }
    
    public synchronized void removeEJBEventListener(EJBEventListener listener) {
        tListeners.remove(listener);
        if(tListeners.isEmpty()) {
            disconnect();
        }
    }
    
    public static EJBEventManager getInstance() {
        return theInstance;
    }
    
      private void connect() {
        try {
            tConnection = tcFactory.createTopicConnection();
            tSession    = tConnection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
            tSub = tSession.createSubscriber(ejbEvents);
            tSub.setMessageListener(this);
            tConnection.start();
        } catch(JMSException jmsex) {
            jmsex.printStackTrace();
            throw new IllegalStateException(jmsex.getMessage());
        }
    }
    
    private void disconnect() {
        try {
            tConnection.stop();
            tSub.close();
            tSession.close();
            tConnection.close();
        } catch(JMSException jmsex) {
            jmsex.printStackTrace();
            throw new IllegalStateException(jmsex.getMessage());
        }
    }

    public void onMessage(Message msg) {
        EJBEvent event = null;
        try {
            event = (EJBEvent)((ObjectMessage)msg).getObject();
        } catch(ClassCastException ccex) {
            ccex.printStackTrace();
            System.err.println("expected javax.jms.ObjectMessage!");
            return;
        } catch(JMSException jmsex) {
            jmsex.printStackTrace();
            return;
        }
        EJBEventListener l = null;
        if(event == null) {
            return;
        }
        for(int i = 0; i < tListeners.size(); i++) {
            l = (EJBEventListener)tListeners.get(i);
            l.notify(event);
        }
    }
    
}
