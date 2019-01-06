package rest;

import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import io.dropwizard.Application;
import rest.health.TemplateHealthCheck;
import rest.resources.test;

public class DirectoryApplication extends Application<DirectoryConfiguration>{
    public static void main(String[] args) throws Exception {
        new DirectoryApplication().run(args);
    }

    @Override
    public String getName() { return "Directory"; }

    @Override
    public void initialize(Bootstrap<DirectoryConfiguration> bootstrap) { }

    @Override
    public void run(DirectoryConfiguration configuration,
                    Environment environment) {
        environment.jersey().register(
                new test());
        environment.healthChecks().register("template",
                new TemplateHealthCheck(configuration.template));
    }

}
